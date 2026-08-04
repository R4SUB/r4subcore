#' Evidence Schema Specification
#'
#' Returns the R4SUB evidence schema as a tidy table: one row per column with
#' its type, whether it is required, the controlled vocabulary (when the column
#' has one), and a short description. This is the published, machine-readable
#' form of the schema that [evidence_schema()] encodes, suitable for
#' documentation, a data dictionary, or a Define-XML style reference.
#'
#' @return A tibble with columns `column`, `type`, `required`,
#'   `allowed_values`, and `description`. `allowed_values` is a comma-separated
#'   string, or `NA` when the column is free text.
#'
#' @examples
#' evidence_schema_spec()
#'
#' @export
evidence_schema_spec <- function() {
  schema <- evidence_schema()

  descriptions <- c(
    run_id           = "Identifier for the run that produced the evidence.",
    study_id         = "Study the evidence belongs to.",
    asset_type       = "Kind of asset the check was run against.",
    asset_id         = "Identifier of the asset, for example a dataset name.",
    source_name      = "Tool or process that produced the evidence.",
    source_version   = "Version of the source tool or process.",
    indicator_id     = "Stable identifier for the check or indicator.",
    indicator_name   = "Human-readable name of the indicator.",
    indicator_domain = "Readiness pillar the indicator contributes to.",
    severity         = "How serious a failure of this check is.",
    result           = "Outcome of the check.",
    metric_value     = "Numeric value behind the result, when there is one.",
    metric_unit      = "Unit for metric_value.",
    message          = "Free-text explanation of the result.",
    location         = "Where the finding sits, for example a variable or file.",
    evidence_payload = "JSON payload with the raw detail behind the result.",
    created_at        = "Time the evidence row was created."
  )

  cols <- names(schema)
  rows <- lapply(cols, function(col) {
    spec <- schema[[col]]
    allowed <- if (is.null(spec$allowed)) NA_character_ else
      paste(spec$allowed, collapse = ", ")

    tibble::tibble(
      column         = col,
      type           = spec$type,
      required       = !isTRUE(spec$nullable),
      allowed_values = allowed,
      description    = unname(descriptions[col])
    )
  })

  do.call(rbind, rows)
}
