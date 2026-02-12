#' Evidence Table Schema Definition
#'
#' Returns the column specification for the R4SUB evidence table. Each element
#' describes a column's expected R type and, where applicable, the set of
#' allowed values.
#'
#' @return A named list. Each element is a list with `type` (character) and
#'   optionally `allowed` (character vector) or `nullable` (logical).
#'
#' @examples
#' str(evidence_schema())
#'
#' @export
evidence_schema <- function() {
  list(
    run_id           = list(type = "character", nullable = FALSE),
    study_id         = list(type = "character", nullable = FALSE),
    asset_type       = list(type = "character", nullable = FALSE,
                            allowed = c("dataset", "define", "program",
                                        "validation", "spec", "other")),
    asset_id         = list(type = "character", nullable = FALSE),
    source_name      = list(type = "character", nullable = FALSE),
    source_version   = list(type = "character", nullable = TRUE),
    indicator_id     = list(type = "character", nullable = FALSE),
    indicator_name   = list(type = "character", nullable = FALSE),
    indicator_domain = list(type = "character", nullable = FALSE,
                            allowed = c("quality", "trace", "risk", "usability")),
    severity         = list(type = "character", nullable = FALSE,
                            allowed = c("info", "low", "medium", "high", "critical")),
    result           = list(type = "character", nullable = FALSE,
                            allowed = c("pass", "fail", "warn", "na")),
    metric_value     = list(type = "double",    nullable = TRUE),
    metric_unit      = list(type = "character", nullable = TRUE),
    message          = list(type = "character", nullable = TRUE),
    location         = list(type = "character", nullable = TRUE),
    evidence_payload = list(type = "character", nullable = TRUE),
    created_at       = list(type = "POSIXct",   nullable = FALSE)
  )
}

#' Canonical Severity Values
#'
#' Maps common severity labels (case-insensitive) to the canonical set.
#'
#' @param x Character vector of severity values.
#' @return Character vector with canonical severity labels.
#'
#' @examples
#' canon_severity(c("HIGH", "Low", "warning", "Error"))
#'
#' @importFrom cli cli_abort
#' @export
canon_severity <- function(x) {
  map <- c(
    "info"     = "info",     "information" = "info",   "note" = "info",
    "low"      = "low",      "minor"       = "low",
    "medium"   = "medium",   "moderate"    = "medium", "warning" = "medium",
    "warn"     = "medium",
    "high"     = "high",     "major"       = "high",   "error" = "high",
    "critical" = "critical", "severe"      = "critical"
  )
  lower <- tolower(trimws(x))
  out <- unname(map[lower])
  bad <- is.na(out) & !is.na(x)
  if (any(bad)) {
    bad_vals <- paste(unique(x[bad]), collapse = ", ")
    allowed_vals <- paste(unique(map), collapse = ", ")
    cli::cli_abort(
      "Unknown severity value(s): {bad_vals}. Expected one of: {allowed_vals} (or common aliases)."
    )
  }
  out
}

#' Canonical Result Values
#'
#' Maps common result/status labels to the canonical set:
#' `pass`, `fail`, `warn`, `na`.
#'
#' @param x Character vector of result values.
#' @return Character vector with canonical result labels.
#'
#' @examples
#' canon_result(c("PASS", "Failed", "Warning", "N/A"))
#'
#' @export
canon_result <- function(x) {
  map <- c(
    "pass"    = "pass",   "passed" = "pass",  "ok" = "pass",  "success" = "pass",
    "fail"    = "fail",   "failed" = "fail",  "error" = "fail",
    "warn"    = "warn",   "warning" = "warn",
    "na"      = "na",     "n/a" = "na",       "not applicable" = "na",
    "missing" = "na"
  )

  lower <- tolower(trimws(x))
  out <- unname(map[lower])
  bad <- is.na(out) & !is.na(x)
  if (any(bad)) {
    bad_vals <- paste(unique(x[bad]), collapse = ", ")
    allowed_vals <- paste(unique(map), collapse = ", ")
    cli::cli_abort(
      "Unknown result value(s): {bad_vals}. Expected one of: {allowed_vals} (or common aliases)."
    )
  }
  out
}

#' Coerce to Evidence Table
#'
#' Takes a data.frame and coerces it into a valid evidence table. Fills in
#' missing nullable columns with `NA` of the correct type and validates
#' controlled vocabulary columns.
#'
#' @param x A data.frame (or tibble) with at least the required evidence
#'   columns.
#' @param ctx An optional [r4sub_run_context]. If provided, `run_id` and
#'   `study_id` are filled from the context when missing.
#' @param ... Additional columns to set (e.g., `asset_type = "validation"`).
#'
#' @return A data.frame conforming to the evidence schema.
#'
#' @examples
#' ctx <- r4sub_run_context("STUDY1", "DEV")
#' df <- data.frame(
#'   asset_type = "validation",
#'   asset_id = "ADSL",
#'   source_name = "pinnacle21",
#'   indicator_id = "P21-001",
#'   indicator_name = "Missing variable",
#'   indicator_domain = "quality",
#'   severity = "high",
#'   result = "fail",
#'   message = "Variable AGEU missing",
#'   stringsAsFactors = FALSE
#' )
#' ev <- as_evidence(df, ctx = ctx)
#'
#' @importFrom cli cli_alert_success
#' @export
as_evidence <- function(x, ctx = NULL, ...) {
  stopifnot(is.data.frame(x))
  schema <- evidence_schema()

  dots <- list(...)
  for (nm in names(dots)) {
    x[[nm]] <- dots[[nm]]
  }

  # Fill from context
  if (!is.null(ctx)) {
    if (!"run_id" %in% names(x) || all(is.na(x$run_id))) {
      x$run_id <- ctx$run_id
    }
    if (!"study_id" %in% names(x) || all(is.na(x$study_id))) {
      x$study_id <- ctx$study_id
    }
  }

  # Fill missing nullable columns
  for (col in names(schema)) {
    spec <- schema[[col]]
    if (!col %in% names(x)) {
      if (isTRUE(spec$nullable)) {
        x[[col]] <- switch(
          spec$type,
          "character" = NA_character_,
          "double"    = NA_real_,
          "POSIXct"   = as.POSIXct(NA),
          NA
        )
      }
    }
  }

  # Fill created_at if missing
  if (!"created_at" %in% names(x) || all(is.na(x$created_at))) {
    x$created_at <- Sys.time()
  }

  # Fill evidence_payload if NA
  if ("evidence_payload" %in% names(x)) {
    x$evidence_payload[is.na(x$evidence_payload)] <- "{}"
  }

  # Coerce types
  for (col in names(schema)) {
    spec <- schema[[col]]
    if (col %in% names(x)) {
      x[[col]] <- switch(
        spec$type,
        "character" = as.character(x[[col]]),
        "double"    = as.double(x[[col]]),
        "POSIXct"   = as.POSIXct(x[[col]]),
        x[[col]]
      )
    }
  }

  # Reorder to schema order
  schema_cols <- names(schema)
  extra_cols <- setdiff(names(x), schema_cols)
  x <- x[, c(intersect(schema_cols, names(x)), extra_cols), drop = FALSE]
  rownames(x) <- NULL

  validate_evidence(x)
  cli::cli_alert_success("Evidence table created: {nrow(x)} row{?s}")
  x
}

#' Validate Evidence Table
#'
#' Checks that a data.frame conforms to the evidence schema. Verifies column
#' presence, types, and controlled vocabulary values.
#'
#' @param ev A data.frame to validate.
#' @return `TRUE` invisibly if valid; throws an error otherwise.
#'
#' @examples
#' \dontrun{
#' validate_evidence(my_evidence)
#' }
#'
#' @importFrom cli cli_abort
#' @export
validate_evidence <- function(ev) {
  stopifnot(is.data.frame(ev))
  schema <- evidence_schema()

  # Check required columns exist
  required <- names(schema)[!vapply(schema, function(s) isTRUE(s$nullable), logical(1))]
  missing_cols <- setdiff(required, names(ev))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Evidence table is missing required column{?s}: {.field {missing_cols}}"
    )
  }

  # Check all schema columns that exist have correct type
  for (col in intersect(names(schema), names(ev))) {
    spec <- schema[[col]]
    vals <- ev[[col]]

    # Type check
    type_ok <- switch(
      spec$type,
      "character" = is.character(vals),
      "double"    = is.numeric(vals),
      "POSIXct"   = inherits(vals, "POSIXct"),
      TRUE
    )
    if (!type_ok) {
      cli::cli_abort(
        "Column {.field {col}} must be {.cls {spec$type}}, got {.cls {class(vals)[1]}}"
      )
    }

    # Nullable check
    if (!isTRUE(spec$nullable) && any(is.na(vals))) {
      cli::cli_abort("Column {.field {col}} must not contain NA values")
    }

    # Allowed values check
    if (!is.null(spec$allowed)) {
      non_na <- vals[!is.na(vals)]
      bad <- setdiff(unique(non_na), spec$allowed)
      if (length(bad) > 0) {
        bad_str <- paste(bad, collapse = ", ")
        allowed_str <- paste(spec$allowed, collapse = ", ")
        cli::cli_abort(
          "Column {.field {col}} has invalid value(s): {bad_str}. Allowed: {allowed_str}"
        )
      }
    }
  }

  invisible(TRUE)
}

#' Bind Evidence Tables
#'
#' Row-binds multiple evidence data.frames after validating each one.
#'
#' @param ... Evidence data.frames to bind.
#' @return A single combined evidence data.frame.
#'
#' @examples
#' \dontrun{
#' combined <- bind_evidence(ev1, ev2)
#' }
#'
#' @importFrom cli cli_alert_success
#' @export
bind_evidence <- function(...) {
  frames <- list(...)
  for (i in seq_along(frames)) {
    validate_evidence(frames[[i]])
  }
  combined <- do.call(rbind, frames)
  rownames(combined) <- NULL
  cli::cli_alert_success("Bound {length(frames)} evidence table{?s}: {nrow(combined)} total row{?s}")
  combined
}

#' Summarize Evidence
#'
#' Returns a summary data.frame with counts grouped by domain, severity,
#' result, and source.
#'
#' @param ev A valid evidence data.frame.
#' @return A data.frame with columns: `indicator_domain`, `severity`, `result`,
#'   `source_name`, and `n`.
#'
#' @examples
#' \dontrun{
#' evidence_summary(ev)
#' }
#'
#' @export
evidence_summary <- function(ev) {
  validate_evidence(ev)

  # Base R aggregation using a separator unlikely to appear in data
  sep <- "\x01"
  groups <- interaction(
    ev$indicator_domain, ev$severity, ev$result, ev$source_name,
    drop = TRUE, sep = sep
  )
  counts <- as.data.frame(table(groups), stringsAsFactors = FALSE)
  if (nrow(counts) == 0) {
    return(data.frame(
      indicator_domain = character(0),
      severity = character(0),
      result = character(0),
      source_name = character(0),
      n = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  parts <- strsplit(counts$groups, sep, fixed = TRUE)
  data.frame(
    indicator_domain = vapply(parts, `[`, character(1), 1),
    severity         = vapply(parts, `[`, character(1), 2),
    result           = vapply(parts, `[`, character(1), 3),
    source_name      = vapply(parts, `[`, character(1), 4),
    n                = as.integer(counts$Freq),
    stringsAsFactors = FALSE
  )
}
