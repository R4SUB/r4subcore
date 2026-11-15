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
    "notice"   = "info",
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

  x <- stamp_evidence_version(x)
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
#' validate_evidence(ev)
#'
#' @importFrom cli cli_abort
#' @export
validate_evidence <- function(ev) {
  stopifnot(is.data.frame(ev))

  # Schema version gate. An absent stamp is tolerated (tables predating
  # versioning, or round-tripped through CSV/JSON, which drop attributes). An
  # unknown or newer stamp is refused; an older one is a warning with a pointer
  # to migrate_evidence().
  v <- attr(ev, "evidence_schema_version")
  if (!is.null(v)) {
    supported <- supported_evidence_schema_versions()
    current <- current_evidence_schema_version()
    if (!v %in% supported) {
      cli::cli_abort(c(
        "Evidence table schema version {.val {v}} is not supported.",
        "i" = "This release of r4subcore understands: {.val {supported}}."
      ))
    }
    if (match(v, supported) < match(current, supported)) {
      cli::cli_warn(c(
        "Evidence table uses an older schema version {.val {v}} (current is {.val {current}}).",
        "i" = "Run {.fn migrate_evidence} to update it."
      ))
    }
  }

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

# Build a collision key from one or more evidence columns. NA is mapped to an
# explicit sentinel so that two rows that are both NA on a key column group
# together, and never collide with a literal "NA" string. Internal.
.evidence_key <- function(ev, keys) {
  parts <- lapply(keys, function(k) {
    v <- as.character(ev[[k]])
    v[is.na(v)] <- "\u0001<NA>\u0001"
    v
  })
  do.call(paste, c(parts, sep = "\u0001"))
}

#' Detect Colliding Evidence Rows
#'
#' Finds rows in an evidence table that share the same key and reports them.
#' By default the key is the `(indicator_id, asset_id, location)` triple that
#' identifies a single finding on a single place in an asset, so more than one
#' row on that key is a duplicate that assembling adapters can introduce. A
#' collision is a *conflict* when the colliding rows disagree on their outcome,
#' that is when they carry more than one distinct value in `conflict_on`
#' (`result` by default).
#'
#' This is purely structural: it groups rows and counts distinct values. It
#' applies no weights, thresholds, or scoring and makes no judgement about which
#' row is correct.
#'
#' @param ev A valid evidence data.frame.
#' @param keys Character vector of column names forming the collision key.
#'   Defaults to `c("indicator_id", "asset_id", "location")`.
#' @param conflict_on Single column name whose distinct values decide whether a
#'   collision is a conflict. Defaults to `"result"`.
#'
#' @return A data.frame with one row per colliding key (keys appearing on more
#'   than one row), ordered by the key columns. It has the `keys` columns plus
#'   `n` (number of rows on the key), `n_distinct` (distinct non-`NA` values of
#'   `conflict_on`), `conflict` (logical, `TRUE` when `n_distinct > 1`), and
#'   `values` (the distinct `conflict_on` values, comma separated). A table with
#'   no collisions yields a zero-row frame with these columns.
#'
#' @seealso [bind_evidence()], which uses this to report and optionally resolve
#'   collisions while combining tables.
#'
#' @examples
#' ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
#' ev <- suppressMessages(as_evidence(
#'   data.frame(
#'     asset_type = "validation", asset_id = rep("ADSL", 3),
#'     source_name = c("p21", "p21", "other"),
#'     indicator_id = c("SD0001", "SD0001", "SD0002"),
#'     indicator_name = c("SD0001", "SD0001", "SD0002"),
#'     indicator_domain = "quality", severity = "high",
#'     result = c("fail", "warn", "pass"), location = c("AGE", "AGE", "SEX"),
#'     stringsAsFactors = FALSE
#'   ),
#'   ctx = ctx
#' ))
#' evidence_collisions(ev)
#'
#' @importFrom cli cli_abort
#' @export
evidence_collisions <- function(ev,
                                keys = c("indicator_id", "asset_id", "location"),
                                conflict_on = "result") {
  validate_evidence(ev)
  if (!is.character(keys) || length(keys) < 1L) {
    cli::cli_abort("{.arg keys} must be a non-empty character vector.")
  }
  if (!is.character(conflict_on) || length(conflict_on) != 1L) {
    cli::cli_abort("{.arg conflict_on} must be a single column name.")
  }
  missing_cols <- setdiff(c(keys, conflict_on), names(ev))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "Column{?s} not found in evidence table: {.field {missing_cols}}"
    )
  }

  empty <- cbind(
    ev[0, keys, drop = FALSE],
    data.frame(n = integer(0), n_distinct = integer(0),
               conflict = logical(0), values = character(0),
               stringsAsFactors = FALSE)
  )
  rownames(empty) <- NULL
  if (nrow(ev) == 0L) {
    return(empty)
  }

  key <- .evidence_key(ev, keys)
  idx_by_key <- split(seq_len(nrow(ev)), key)
  dup <- idx_by_key[lengths(idx_by_key) > 1L]
  if (length(dup) == 0L) {
    return(empty)
  }

  rows <- lapply(dup, function(idx) {
    cvals <- ev[[conflict_on]][idx]
    distinct <- unique(cvals[!is.na(cvals)])
    cbind(
      ev[idx[1L], keys, drop = FALSE],
      data.frame(
        n          = length(idx),
        n_distinct = length(distinct),
        conflict   = length(distinct) > 1L,
        values     = paste(sort(distinct), collapse = ", "),
        stringsAsFactors = FALSE
      )
    )
  })
  out <- do.call(rbind, rows)
  out <- out[do.call(order, out[keys]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Bind Evidence Tables
#'
#' Combines several R4SUB evidence data.frames -- for example the outputs of
#' different adapters or indicators across the ecosystem -- into one validated
#' evidence table. Each input is validated against the evidence schema, the
#' tables are row-bound, and colliding rows (more than one row sharing the same
#' key) are detected and reported. The combined table carries a collision report
#' in its `evidence_collisions` attribute, and duplicates can optionally be
#' resolved.
#'
#' The default `on_duplicate = "keep"` preserves every row, so the combined
#' table is exactly the row-bind of the inputs; the collision report is attached
#' either way. Resolution, when requested, keeps the first or last row on each
#' key, or aborts. This is purely structural: it never scores, weights, or
#' decides which of two conflicting rows is correct.
#'
#' @param ... Evidence data.frames to bind, or a single list of them.
#' @param keys Character vector of columns forming the collision key, passed to
#'   [evidence_collisions()]. Defaults to
#'   `c("indicator_id", "asset_id", "location")`.
#' @param on_duplicate How to handle keys that appear on more than one row:
#'   `"keep"` (default) keeps all rows, `"first"` keeps the first row on each
#'   key, `"last"` keeps the last, and `"error"` aborts if any key collides.
#'
#' @return A single combined evidence data.frame with an `evidence_collisions`
#'   attribute holding the collision report (see [evidence_collisions()]).
#'
#' @seealso [evidence_collisions()].
#'
#' @examples
#' ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
#' make_ev <- function(ind_id) {
#'   suppressMessages(as_evidence(
#'     data.frame(
#'       asset_type = "validation", asset_id = "ADSL",
#'       source_name = "pinnacle21", indicator_id = ind_id,
#'       indicator_name = ind_id, indicator_domain = "quality",
#'       severity = "low", result = "pass",
#'       stringsAsFactors = FALSE
#'     ),
#'     ctx = ctx
#'   ))
#' }
#' ev1 <- make_ev("IND-001")
#' ev2 <- make_ev("IND-002")
#' combined <- suppressMessages(bind_evidence(ev1, ev2))
#' nrow(combined)
#' attr(combined, "evidence_collisions")
#'
#' @importFrom cli cli_alert_success
#' @importFrom rlang arg_match
#' @export
bind_evidence <- function(...,
                          keys = c("indicator_id", "asset_id", "location"),
                          on_duplicate = c("keep", "first", "last", "error")) {
  on_duplicate <- rlang::arg_match(on_duplicate)
  frames <- list(...)
  # Accept a single list of frames, e.g. bind_evidence(list(ev1, ev2)).
  if (length(frames) == 1L && is.list(frames[[1]]) &&
      !is.data.frame(frames[[1]])) {
    frames <- frames[[1]]
  }
  if (length(frames) == 0L) {
    cli::cli_abort("{.fn bind_evidence} needs at least one evidence table.")
  }
  for (i in seq_along(frames)) {
    validate_evidence(frames[[i]])
  }
  combined <- do.call(rbind, frames)
  rownames(combined) <- NULL

  # rbind drops attributes, so re-stamp. Warn if the inputs mix schema versions.
  versions <- unique(stats::na.omit(
    vapply(frames, evidence_schema_version, character(1))
  ))
  if (length(versions) > 1L) {
    cli::cli_warn(c(
      "Binding evidence tables with different schema versions: {.val {versions}}.",
      "i" = "The result is stamped {.val {current_evidence_schema_version()}}; run {.fn migrate_evidence} first to be explicit."
    ))
  }
  combined <- stamp_evidence_version(combined)
  n_frames <- length(frames)

  # Detect collisions on the combined table (only when every key column exists).
  report <- NULL
  if (all(keys %in% names(combined))) {
    report <- evidence_collisions(combined, keys = keys)
  }
  n_dup_keys  <- if (is.null(report)) 0L else nrow(report)
  n_conflicts <- if (is.null(report)) 0L else sum(report$conflict)

  if (n_dup_keys > 0L && on_duplicate == "error") {
    cli::cli_abort(c(
      "Evidence tables collide on {n_dup_keys} key{?s} ({n_conflicts} with differing {.field result}).",
      "i" = "Inspect them with {.fn evidence_collisions}, or pass {.code on_duplicate = \"first\"}/{.code \"last\"} to resolve."
    ))
  }

  if (n_dup_keys > 0L && on_duplicate %in% c("first", "last")) {
    key <- .evidence_key(combined, keys)
    keep <- if (on_duplicate == "first") {
      !duplicated(key)
    } else {
      !duplicated(key, fromLast = TRUE)
    }
    combined <- combined[keep, , drop = FALSE]
    rownames(combined) <- NULL
    combined <- stamp_evidence_version(combined)
  }

  attr(combined, "evidence_collisions") <- report

  cli::cli_alert_success(
    "Bound {n_frames} evidence table{?s}: {nrow(combined)} total row{?s}"
  )
  if (n_dup_keys > 0L) {
    cli::cli_alert_warning(
      "{n_dup_keys} colliding key{?s} ({n_conflicts} with differing {.field result}); on_duplicate = {.val {on_duplicate}}."
    )
  }
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
#' evidence_summary(ev)
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
