#' Validate Indicator Metadata
#'
#' Checks that an indicator definition list is well-formed.
#'
#' @param indicator A list with required fields: `indicator_id`, `domain`,
#'   `description`. Optional fields: `expected_inputs`, `default_thresholds`,
#'   `tags`.
#' @return `TRUE` invisibly if valid; throws an error otherwise.
#'
#' @examples
#' validate_indicator(list(
#'   indicator_id = "P21-001",
#'   domain = "quality",
#'   description = "Missing required variable"
#' ))
#'
#' @importFrom cli cli_abort
#' @export
validate_indicator <- function(indicator) {
  if (!is.list(indicator)) {
    cli::cli_abort("Indicator must be a list")
  }
  required <- c("indicator_id", "domain", "description")
  missing_fields <- setdiff(required, names(indicator))
  if (length(missing_fields) > 0) {
    cli::cli_abort(
      "Indicator is missing required field{?s}: {.field {missing_fields}}"
    )
  }
  valid_domains <- c("quality", "trace", "risk", "usability")
  if (!indicator$domain %in% valid_domains) {
    cli::cli_abort(
      "Indicator domain must be one of {.val {valid_domains}}, got {.val {indicator$domain}}"
    )
  }
  invisible(TRUE)
}

# Internal indicator registry (package-level environment)
.indicator_registry <- new.env(parent = emptyenv())

#' Register an Indicator
#'
#' Adds an indicator definition to the local in-memory registry.
#'
#' @param indicator_id Character. Stable identifier for the indicator.
#' @param domain Character. One of `"quality"`, `"trace"`, `"risk"`,
#'   `"usability"`.
#' @param description Character. Human-readable description.
#' @param expected_inputs Character vector. Evidence source types this
#'   indicator expects.
#' @param default_thresholds Named numeric vector. Optional thresholds.
#' @param tags Character vector. Optional tags (e.g., `"define"`, `"adam"`).
#'
#' @return The indicator definition list, invisibly.
#'
#' @examples
#' register_indicator(
#'   indicator_id = "P21-001",
#'   domain = "quality",
#'   description = "Required variable is missing from dataset"
#' )
#'
#' @importFrom cli cli_alert_success
#' @export
register_indicator <- function(indicator_id,
                               domain,
                               description,
                               expected_inputs = character(0),
                               default_thresholds = numeric(0),
                               tags = character(0)) {
  ind <- list(
    indicator_id = indicator_id,
    domain = domain,
    description = description,
    expected_inputs = expected_inputs,
    default_thresholds = default_thresholds,
    tags = tags
  )
  validate_indicator(ind)
  assign(indicator_id, ind, envir = .indicator_registry)
  cli::cli_alert_success("Registered indicator: {.val {indicator_id}}")
  invisible(ind)
}

#' List Registered Indicators
#'
#' Returns all indicators currently in the local registry.
#'
#' @return A list of indicator definition lists.
#' @noRd
list_indicators <- function() {
  ids <- ls(envir = .indicator_registry)
  lapply(ids, function(id) get(id, envir = .indicator_registry))
}

#' Get an Indicator by ID
#'
#' Retrieves an indicator from the local registry.
#'
#' @param indicator_id Character. The indicator ID to look up.
#' @return The indicator definition list, or `NULL` if not found.
#' @noRd
get_indicator <- function(indicator_id) {
  if (exists(indicator_id, envir = .indicator_registry, inherits = FALSE)) {
    get(indicator_id, envir = .indicator_registry)
  } else {
    NULL
  }
}
