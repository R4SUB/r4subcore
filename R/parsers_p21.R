#' Parse Pinnacle21 Output to Evidence
#'
#' Converts a data.frame of Pinnacle21-style validation results into the
#' standard evidence table format. Column names are detected case-insensitively.
#'
#' @param p21_df A data.frame containing Pinnacle21 validation output. Expected
#'   columns (case-insensitive): `Rule` (or `Rule ID`), `Message`, `Severity`,
#'   `Dataset`, `Variable`, `Result` (or `Status`).
#' @param ctx A [r4sub_run_context] providing run and study metadata.
#' @param asset_type Character. Asset type label. Default: `"validation"`.
#' @param source_version Character or `NULL`. Version of the P21 tool.
#' @param default_domain Character. Indicator domain. Default: `"quality"`.
#'
#' @return A data.frame conforming to the evidence schema.
#'
#' @examples
#' p21_raw <- data.frame(
#'   Rule = c("SD0001", "SD0002"),
#'   Message = c("Missing variable label", "Invalid format"),
#'   Severity = c("Error", "Warning"),
#'   Dataset = c("ADSL", "ADAE"),
#'   Variable = c("AGE", "AESTDTC"),
#'   Status = c("Failed", "Warning"),
#'   stringsAsFactors = FALSE
#' )
#' ctx <- r4sub_run_context("STUDY1", "DEV")
#' ev <- p21_to_evidence(p21_raw, ctx)
#'
#' @importFrom cli cli_alert_warning cli_alert_info
#' @export
p21_to_evidence <- function(p21_df,
                            ctx,
                            asset_type = "validation",
                            source_version = NULL,
                            default_domain = "quality") {
  stopifnot(is.data.frame(p21_df), inherits(ctx, "r4sub_run_context"))

  # --- Flexible column detection ----
  orig_names <- names(p21_df)
  lower_names <- tolower(orig_names)

  find_col <- function(patterns) {
    for (pat in patterns) {
      idx <- grep(pat, lower_names)
      if (length(idx) > 0) return(orig_names[idx[1]])
    }
    NULL
  }

  col_rule     <- find_col(c("^rule.?id$", "^rule$", "^ruleid$", "^check.?id$"))
  col_message  <- find_col(c("^message$", "^description$", "^msg$"))
  col_severity <- find_col(c("^severity$", "^level$", "^priority$"))
  col_dataset  <- find_col(c("^dataset$", "^domain$", "^table$"))
  col_variable <- find_col(c("^variable$", "^column$", "^field$"))
  col_result   <- find_col(c("^result$", "^status$", "^outcome$"))

  n <- nrow(p21_df)

  # --- Build evidence columns ----
  # Rule / indicator_id
  if (!is.null(col_rule)) {
    rule_vals <- as.character(p21_df[[col_rule]])
  } else {
    cli::cli_alert_warning("No rule/ID column found; generating indicator IDs from row hash")
    rule_vals <- rep(NA_character_, n)
  }

  # Message
  if (!is.null(col_message)) {
    msg_vals <- as.character(p21_df[[col_message]])
  } else {
    msg_vals <- rep(NA_character_, n)
  }

  # indicator_id: use rule if available, otherwise hash rule+message
  indicator_id <- vapply(seq_len(n), function(i) {
    if (!is.na(rule_vals[i]) && nzchar(rule_vals[i])) {
      rule_vals[i]
    } else {
      hash_id(
        if (!is.na(msg_vals[i])) msg_vals[i] else as.character(i),
        prefix = "IND"
      )
    }
  }, character(1))

  indicator_name <- ifelse(
    !is.na(rule_vals) & nzchar(rule_vals),
    rule_vals,
    ifelse(!is.na(msg_vals), substr(msg_vals, 1, 80), "Unknown")
  )

  # Severity
  if (!is.null(col_severity)) {
    severity <- canon_severity(as.character(p21_df[[col_severity]]))
  } else {
    severity <- rep("medium", n)
  }

  # Result
  if (!is.null(col_result)) {
    result <- canon_result(as.character(p21_df[[col_result]]))
  } else {
    result <- rep("fail", n)
  }

  # Location: DATASET:VARIABLE
  ds_vals <- if (!is.null(col_dataset)) as.character(p21_df[[col_dataset]]) else rep(NA_character_, n)
  var_vals <- if (!is.null(col_variable)) as.character(p21_df[[col_variable]]) else rep(NA_character_, n)

  location <- vapply(seq_len(n), function(i) {
    parts <- c(ds_vals[i], var_vals[i])
    parts <- parts[!is.na(parts) & nzchar(parts)]
    if (length(parts) == 0) NA_character_ else paste(parts, collapse = ":")
  }, character(1))

  # asset_id: dataset name if available, else "unknown"
  asset_id <- ifelse(!is.na(ds_vals) & nzchar(ds_vals), ds_vals, "unknown")

  # evidence_payload: serialize each original row as JSON
  evidence_payload <- vapply(seq_len(n), function(i) {
    json_safely(as.list(p21_df[i, , drop = FALSE]))
  }, character(1))

  ev_df <- data.frame(
    run_id           = ctx$run_id,
    study_id         = ctx$study_id,
    asset_type       = asset_type,
    asset_id         = asset_id,
    source_name      = "pinnacle21",
    source_version   = if (!is.null(source_version)) source_version else NA_character_,
    indicator_id     = indicator_id,
    indicator_name   = indicator_name,
    indicator_domain = default_domain,
    severity         = severity,
    result           = result,
    metric_value     = NA_real_,
    metric_unit      = NA_character_,
    message          = msg_vals,
    location         = location,
    evidence_payload = evidence_payload,
    created_at       = Sys.time(),
    stringsAsFactors = FALSE
  )

  cli::cli_alert_info("Parsed {n} row{?s} from Pinnacle21 output")
  as_evidence(ev_df, ctx = ctx)
}
