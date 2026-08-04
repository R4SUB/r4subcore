#' Evidence Schema Version
#'
#' The evidence table carries a schema version so a table written by one release
#' of the ecosystem can be recognized, validated, and migrated by another.
#' `evidence_schema_version()` with no argument returns the version this release
#' writes. Given an evidence table, it returns the version stamped on that table,
#' or `NA` when the table predates versioning.
#'
#' @param x Optional. An evidence data.frame. When supplied, the stamped version
#'   of that table is returned instead of the current package version.
#'
#' @return A single version string, or `NA_character_` for an unstamped table.
#'
#' @seealso [migrate_evidence()] to upgrade an older table.
#'
#' @examples
#' evidence_schema_version()
#'
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
#' evidence_schema_version(ev)
#'
#' @export
evidence_schema_version <- function(x = NULL) {
  if (is.null(x)) {
    return(current_evidence_schema_version())
  }
  v <- attr(x, "evidence_schema_version")
  if (is.null(v)) NA_character_ else as.character(v)
}

# The schema version this release writes. Bump when the evidence schema changes
# (a new column, a changed vocabulary), and add a migration step below.
current_evidence_schema_version <- function() {
  "1.0.0"
}

# Versions this release can read and migrate from, ordered oldest to newest.
supported_evidence_schema_versions <- function() {
  c("1.0.0")
}

# Stamp a table with a schema version. Internal.
stamp_evidence_version <- function(x, version = current_evidence_schema_version()) {
  attr(x, "evidence_schema_version") <- version
  x
}

# Registry of stepwise migrations, keyed "from->to". Each entry is a function
# that takes a table at `from` and returns it shaped for `to`. Empty until the
# schema first changes; `migrate_evidence()` then walks these in order.
evidence_migration_steps <- function() {
  list(
    # "1.0.0->1.1.0" = function(x) { x$new_column <- NA_character_; x }
  )
}

#' Migrate an Evidence Table to a Newer Schema Version
#'
#' Upgrades an evidence table written by an older release to the requested
#' schema version, applying each intervening migration step in order. A table
#' with no stamp is treated as the earliest known version. A table already at
#' the target is returned unchanged apart from the version stamp.
#'
#' @param x An evidence data.frame.
#' @param to Target schema version. Defaults to the current version.
#'
#' @return The migrated evidence data.frame, stamped with `to`.
#'
#' @seealso [evidence_schema_version()].
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
#' ev <- migrate_evidence(ev)
#' evidence_schema_version(ev)
#'
#' @export
migrate_evidence <- function(x, to = current_evidence_schema_version()) {
  stopifnot(is.data.frame(x))
  supported <- supported_evidence_schema_versions()

  if (!to %in% supported) {
    cli::cli_abort(c(
      "Cannot migrate to unknown schema version {.val {to}}.",
      "i" = "This release understands: {.val {supported}}."
    ))
  }

  from <- evidence_schema_version(x)
  if (is.na(from)) {
    # A table without a stamp predates versioning; treat it as the earliest.
    from <- supported[1]
  }
  if (!from %in% supported) {
    cli::cli_abort(c(
      "Evidence table has unknown schema version {.val {from}}.",
      "i" = "This release understands: {.val {supported}}. Upgrade r4subcore."
    ))
  }

  from_i <- match(from, supported)
  to_i   <- match(to, supported)
  if (from_i > to_i) {
    cli::cli_abort(
      "Evidence table version {.val {from}} is newer than the target {.val {to}}."
    )
  }

  steps <- evidence_migration_steps()
  for (i in seq_len(to_i - from_i)) {
    step_from <- supported[from_i + i - 1L]
    step_to   <- supported[from_i + i]
    fn <- steps[[paste0(step_from, "->", step_to)]]
    if (!is.null(fn)) {
      x <- fn(x)
    }
  }

  stamp_evidence_version(x, to)
}
