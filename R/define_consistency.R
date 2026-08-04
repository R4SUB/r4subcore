#' Extract the Variables Declared in a Define-XML File
#'
#' Returns one row per variable declared in Define-XML, following the
#' `ItemGroupDef` to `ItemRef` to `ItemDef` structure: the dataset name comes
#' from the `ItemGroupDef`, and the variable name from the `ItemDef` its
#' `ItemRef` points at. Elements are matched by local name, so the result does
#' not depend on how the file mixes the ODM and `def:` namespaces.
#'
#' @param file Path to a Define-XML file.
#'
#' @return A data.frame with columns `dataset` and `variable`.
#'
#' @seealso [check_define_consistency()].
#'
#' @importFrom xml2 read_xml xml_find_all xml_attr
#' @export
define_variables <- function(file) {
  stopifnot(is.character(file), length(file) == 1L, file.exists(file))
  doc <- xml2::read_xml(file)

  # OID -> variable name from every ItemDef.
  item_defs <- xml2::xml_find_all(doc, "//*[local-name()='ItemDef']")
  oid_to_var <- list()
  for (id_node in item_defs) {
    oid <- xml2::xml_attr(id_node, "OID")
    nm  <- xml2::xml_attr(id_node, "Name")
    if (!is.na(oid)) oid_to_var[[oid]] <- nm
  }

  groups <- xml2::xml_find_all(doc, "//*[local-name()='ItemGroupDef']")
  rows <- list()
  for (ig in groups) {
    ds <- xml2::xml_attr(ig, "Name")
    for (ir in xml2::xml_find_all(ig, ".//*[local-name()='ItemRef']")) {
      oid <- xml2::xml_attr(ir, "ItemOID")
      var <- if (!is.null(oid) && !is.na(oid) && !is.null(oid_to_var[[oid]])) {
        oid_to_var[[oid]]
      } else {
        NA_character_
      }
      rows[[length(rows) + 1L]] <- data.frame(
        dataset  = if (!is.na(ds)) ds else NA_character_,
        variable = var,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0L) {
    return(data.frame(dataset = character(0), variable = character(0),
                      stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  out[!is.na(out$variable) & !is.na(out$dataset), , drop = FALSE]
}

# Normalize a (dataset, variable) table: find the columns case-insensitively,
# and upper-case and trim the values so the comparison is case-insensitive.
normalize_var_table <- function(x, side) {
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg {side}} must be a data.frame with dataset and variable columns.")
  }
  lower <- tolower(names(x))
  ds_col  <- names(x)[match("dataset", lower)]
  var_col <- names(x)[match("variable", lower)]
  if (is.na(ds_col) || is.na(var_col)) {
    cli::cli_abort("{.arg {side}} must have {.field dataset} and {.field variable} columns.")
  }
  data.frame(
    dataset  = toupper(trimws(as.character(x[[ds_col]]))),
    variable = toupper(trimws(as.character(x[[var_col]]))),
    stringsAsFactors = FALSE
  )
}

#' Check Define-XML Against Dataset Contents
#'
#' Compares the variables declared in Define-XML against the variables actually
#' present in the datasets, and emits evidence for each: a match, a variable
#' documented in Define but absent from the data, or a variable present in the
#' data but not documented. Drift between Define-XML and the data is a common
#' submission problem that reviewers notice quickly, and this makes it a scored
#' indicator. The comparison is case-insensitive on dataset and variable names.
#'
#' @param define Either a path to a Define-XML file, or a data.frame with
#'   `dataset` and `variable` columns (as returned by [define_variables()]).
#' @param data A data.frame with `dataset` and `variable` columns listing the
#'   variables actually present in each dataset.
#' @param ctx A [r4sub_run_context].
#' @param source_name,source_version Provenance recorded on the evidence.
#'
#' @return A data.frame conforming to the evidence schema, one row per variable
#'   checked, under indicator `Q-DEFINE-CONSIST`.
#'
#' @seealso [define_variables()], [define_xml_to_evidence()].
#'
#' @examples
#' ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
#' define <- data.frame(
#'   dataset  = c("ADSL", "ADSL", "ADSL"),
#'   variable = c("USUBJID", "AGE", "SEX")
#' )
#' data <- data.frame(
#'   dataset  = c("ADSL", "ADSL", "ADSL"),
#'   variable = c("USUBJID", "AGE", "RACE")  # SEX missing, RACE undocumented
#' )
#' ev <- suppressMessages(check_define_consistency(define, data, ctx))
#' ev[, c("asset_id", "result", "message")]
#'
#' @importFrom cli cli_alert_info cli_abort
#' @export
check_define_consistency <- function(define, data, ctx,
                                     source_name = "define_vs_data",
                                     source_version = NULL) {
  stopifnot(inherits(ctx, "r4sub_run_context"))

  dv <- normalize_var_table(
    if (is.data.frame(define)) define else define_variables(define),
    "define"
  )
  dd <- normalize_var_table(data, "data")

  dv_key <- paste(dv$dataset, dv$variable, sep = "\x01")
  dd_key <- paste(dd$dataset, dd$variable, sep = "\x01")
  keys <- union(dv_key, dd_key)

  key_ds  <- vapply(strsplit(keys, "\x01", fixed = TRUE), `[`, character(1), 1)
  key_var <- vapply(strsplit(keys, "\x01", fixed = TRUE), `[`, character(1), 2)

  in_define <- keys %in% dv_key
  in_data   <- keys %in% dd_key

  status <- ifelse(in_define & in_data, "match",
             ifelse(in_define & !in_data, "define_only", "data_only"))

  result <- ifelse(status == "match", "pass", "fail")
  severity <- ifelse(status == "match", "info", "high")
  message <- ifelse(
    status == "match",
    sprintf("%s.%s is documented and present", key_ds, key_var),
    ifelse(
      status == "define_only",
      sprintf("%s.%s is documented in Define-XML but absent from the data",
              key_ds, key_var),
      sprintf("%s.%s is present in the data but not documented in Define-XML",
              key_ds, key_var)
    )
  )

  ev_df <- data.frame(
    asset_type       = "define",
    asset_id         = key_ds,
    source_name      = source_name,
    source_version   = if (!is.null(source_version)) source_version else NA_character_,
    indicator_id     = "Q-DEFINE-CONSIST",
    indicator_name   = "Define-XML matches dataset contents",
    indicator_domain = "quality",
    severity         = severity,
    result           = result,
    metric_value     = ifelse(result == "pass", 1, 0),
    metric_unit      = "match",
    message          = message,
    location         = paste(key_ds, key_var, sep = ":"),
    stringsAsFactors = FALSE
  )

  n_mismatch <- sum(result == "fail")
  cli::cli_alert_info(
    "Checked {length(keys)} variable{?s}: {n_mismatch} mismatch{?es}"
  )
  as_evidence(ev_df, ctx = ctx)
}
