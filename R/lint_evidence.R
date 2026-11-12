#' Lint an Evidence Table
#'
#' Checks an evidence table against the R4SUB evidence schema and a handful of
#' structural best practices, and reports *every* issue it finds at once as a
#' tidy data.frame. This is the non-fatal counterpart to [validate_evidence()]:
#' where `validate_evidence()` throws on the first problem (and is the right
#' gate before scoring), `lint_evidence()` never throws on a malformed table,
#' so you can see the full picture of what is wrong in one pass.
#'
#' Every check is structural. The linter reuses the schema from
#' [evidence_schema()] and the version machinery behind
#' [evidence_schema_version()]; it invents no scoring, weights, thresholds, or
#' controlled vocabularies of its own, and it never judges whether a finding is
#' good or bad for a submission. The checks are:
#'
#' \describe{
#'   \item{`error`}{Issues that would also make [validate_evidence()] fail: a
#'     required column missing (`missing_required_column`), a schema column of
#'     the wrong type (`wrong_type`), an `NA` in a non-nullable column
#'     (`na_in_required`), a value outside a column's controlled vocabulary
#'     (`invalid_value`), or a schema-version stamp this release does not know
#'     (`unknown_schema_version`).}
#'   \item{`warning`}{A schema-version stamp older than the current one
#'     (`outdated_schema_version`), or an `evidence_payload` cell that is neither
#'     empty nor valid JSON (`invalid_payload_json`).}
#'   \item{`note`}{No schema-version stamp at all (`missing_schema_version`);
#'     more than one row sharing an `(indicator_id, asset_id)` key, which
#'     [evidence_diff()] would collapse (`duplicate_key`); a `metric_value`
#'     with no `metric_unit` (`metric_value_without_unit`) or a `metric_unit`
#'     with no `metric_value` (`metric_unit_without_value`).}
#' }
#'
#' @param ev A data.frame to lint. It need not be a valid evidence table; that
#'   is the point.
#' @param checks Optional character vector of check codes to run (see the codes
#'   in the descriptions above). The default, `NULL`, runs them all. Unknown
#'   codes are rejected.
#'
#' @return A [tibble][tibble::tibble] with one row per issue and the columns
#'   `code` (the check that fired), `level` (`error`, `warning`, or `note`),
#'   `field` (the column the issue concerns, or `NA` for a table-level issue),
#'   `n` (the number of rows affected, or `NA` for a table-level issue), and
#'   `message` (a human-readable description). A clean table returns a zero-row
#'   tibble with those columns. Rows are ordered most serious first. The result
#'   carries a `"summary"` attribute: a named integer vector counting findings
#'   at each level.
#'
#' @seealso [validate_evidence()] for the strict gate, and [evidence_schema()]
#'   for the schema the checks are drawn from.
#'
#' @examples
#' ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
#' ev <- suppressMessages(as_evidence(
#'   data.frame(
#'     asset_type = "validation", asset_id = "ADSL",
#'     source_name = "pinnacle21", indicator_id = "SD0001",
#'     indicator_name = "SD0001", indicator_domain = "quality",
#'     severity = "high", result = "fail",
#'     stringsAsFactors = FALSE
#'   ),
#'   ctx = ctx
#' ))
#' # A valid table lints clean.
#' lint_evidence(ev)
#'
#' # A hand-built table with problems reports them all at once.
#' bad <- data.frame(
#'   run_id = "R1", study_id = "S1", asset_type = "dataset",
#'   asset_id = "ADSL", source_name = "x", indicator_id = "I1",
#'   indicator_name = "I1", indicator_domain = "quality",
#'   severity = "sky-high", result = "pass",
#'   created_at = Sys.time(), stringsAsFactors = FALSE
#' )
#' lint_evidence(bad)
#'
#' @importFrom cli cli_abort
#' @importFrom tibble tibble
#' @importFrom jsonlite fromJSON
#' @export
lint_evidence <- function(ev, checks = NULL) {
  if (!is.data.frame(ev)) {
    cli::cli_abort("{.arg ev} must be a data.frame.")
  }

  all_codes <- c(
    "missing_required_column", "wrong_type", "na_in_required",
    "invalid_value", "unknown_schema_version", "outdated_schema_version",
    "invalid_payload_json", "missing_schema_version", "duplicate_key",
    "metric_value_without_unit", "metric_unit_without_value"
  )
  if (is.null(checks)) {
    checks <- all_codes
  } else {
    if (!is.character(checks)) {
      cli::cli_abort("{.arg checks} must be a character vector of check codes.")
    }
    bad <- setdiff(checks, all_codes)
    if (length(bad)) {
      cli::cli_abort(c(
        "Unknown check code{?s}: {.val {bad}}.",
        "i" = "Known codes: {.val {all_codes}}."
      ))
    }
  }

  schema <- evidence_schema()
  findings <- list()
  add <- function(code, level, field, n, message) {
    if (!code %in% checks) return(invisible())
    findings[[length(findings) + 1L]] <<- lint_finding(
      code, level, field, n, message
    )
  }

  # --- Schema-version stamp -------------------------------------------------
  v <- attr(ev, "evidence_schema_version")
  supported <- supported_evidence_schema_versions()
  current <- current_evidence_schema_version()
  if (is.null(v)) {
    add("missing_schema_version", "note", NA_character_, NA_integer_,
        "Evidence table has no schema-version stamp; as_evidence() stamps one.")
  } else if (!v %in% supported) {
    add("unknown_schema_version", "error", NA_character_, NA_integer_,
        paste0("Schema version '", v, "' is not one this release understands (",
               paste(supported, collapse = ", "), ")."))
  } else if (match(v, supported) < match(current, supported)) {
    add("outdated_schema_version", "warning", NA_character_, NA_integer_,
        paste0("Schema version '", v, "' is older than the current '", current,
               "'; run migrate_evidence()."))
  }

  # --- Column presence, type, NA, vocabulary --------------------------------
  required <- names(schema)[!vapply(schema, function(s) isTRUE(s$nullable),
                                    logical(1))]
  for (col in setdiff(required, names(ev))) {
    add("missing_required_column", "error", col, NA_integer_,
        paste0("Required column '", col, "' is missing."))
  }

  for (col in intersect(names(schema), names(ev))) {
    spec <- schema[[col]]
    vals <- ev[[col]]

    type_ok <- switch(
      spec$type,
      "character" = is.character(vals),
      "double"    = is.numeric(vals),
      "POSIXct"   = inherits(vals, "POSIXct"),
      TRUE
    )
    if (!type_ok) {
      add("wrong_type", "error", col, NA_integer_,
          paste0("Column '", col, "' should be ", spec$type, " but is ",
                 class(vals)[1], "."))
    }

    if (!isTRUE(spec$nullable)) {
      n_na <- sum(is.na(vals))
      if (n_na > 0) {
        add("na_in_required", "error", col, n_na,
            paste0("Column '", col, "' is required but has ", n_na,
                   " NA value", if (n_na == 1) "" else "s", "."))
      }
    }

    if (!is.null(spec$allowed)) {
      non_na <- as.character(vals[!is.na(vals)])
      bad <- setdiff(unique(non_na), spec$allowed)
      if (length(bad)) {
        n_bad <- sum(non_na %in% bad)
        add("invalid_value", "error", col, n_bad,
            paste0("Column '", col, "' has value(s) outside its vocabulary: ",
                   paste(bad, collapse = ", "), ". Allowed: ",
                   paste(spec$allowed, collapse = ", "), "."))
      }
    }
  }

  # --- evidence_payload JSON ------------------------------------------------
  if ("evidence_payload" %in% names(ev) &&
      is.character(ev$evidence_payload)) {
    p <- ev$evidence_payload
    consider <- !is.na(p) & nzchar(p)
    if (any(consider)) {
      ok <- vapply(p[consider], lint_is_json, logical(1), USE.NAMES = FALSE)
      n_bad <- sum(!ok)
      if (n_bad > 0) {
        add("invalid_payload_json", "warning", "evidence_payload", n_bad,
            paste0(n_bad, " evidence_payload cell",
                   if (n_bad == 1) " is" else "s are", " not valid JSON."))
      }
    }
  }

  # --- Duplicate (indicator_id, asset_id) keys ------------------------------
  if (all(c("indicator_id", "asset_id") %in% names(ev)) && nrow(ev) > 0) {
    k <- paste(as.character(ev$indicator_id), as.character(ev$asset_id),
               sep = "\x01")
    tab <- table(k)
    n_dup_keys <- sum(tab > 1L)
    if (n_dup_keys > 0) {
      n_rows <- sum(tab[tab > 1L])
      add("duplicate_key", "note", NA_character_, n_rows,
          paste0(n_dup_keys, " (indicator_id, asset_id) key",
                 if (n_dup_keys == 1) "" else "s", " appear on ", n_rows,
                 " rows; evidence_diff() collapses these to the worst row."))
    }
  }

  # --- metric_value / metric_unit completeness ------------------------------
  if (all(c("metric_value", "metric_unit") %in% names(ev))) {
    val_na  <- is.na(ev$metric_value)
    unit_na <- is.na(ev$metric_unit)
    n_vnu <- sum(!val_na & unit_na)
    if (n_vnu > 0) {
      add("metric_value_without_unit", "note", "metric_unit", n_vnu,
          paste0(n_vnu, " row", if (n_vnu == 1) " has" else "s have",
                 " a metric_value but no metric_unit."))
    }
    n_unv <- sum(val_na & !unit_na)
    if (n_unv > 0) {
      add("metric_unit_without_value", "note", "metric_value", n_unv,
          paste0(n_unv, " row", if (n_unv == 1) " has" else "s have",
                 " a metric_unit but no metric_value."))
    }
  }

  out <- if (length(findings)) {
    do.call(rbind, findings)
  } else {
    lint_finding(character(0), character(0), character(0),
                 integer(0), character(0))
  }

  level_rank <- c(error = 1L, warning = 2L, note = 3L)
  ord <- order(level_rank[out$level], out$code)
  out <- out[ord, , drop = FALSE]
  rownames(out) <- NULL

  attr(out, "summary") <- c(
    error   = sum(out$level == "error"),
    warning = sum(out$level == "warning"),
    note    = sum(out$level == "note")
  )
  out
}

# One finding as a one-row tibble (or a zero-row template).
lint_finding <- function(code, level, field, n, message) {
  tibble::tibble(
    code    = as.character(code),
    level   = as.character(level),
    field   = as.character(field),
    n       = as.integer(n),
    message = as.character(message)
  )
}

# TRUE when a single string parses as JSON. Used only for the payload check.
lint_is_json <- function(s) {
  tryCatch({
    jsonlite::fromJSON(s)
    TRUE
  }, error = function(e) FALSE)
}
