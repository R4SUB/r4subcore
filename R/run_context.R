#' Create a Run Context
#'
#' A run context captures metadata for a particular evidence collection run.
#' It provides a unique `run_id`, study identifier, environment label, and
#' timestamps used throughout evidence ingestion.
#'
#' @param study_id Character. Study identifier (e.g., `"ABC123"`).
#' @param environment Character. One of `"DEV"`, `"UAT"`, `"PROD"`.
#' @param user Character or `NULL`. Username; defaults to system user.
#' @param run_id Character or `NULL`. If `NULL`, a unique ID is generated.
#' @param timestamp POSIXct. Defaults to current time.
#'
#' @return A list of class `r4sub_run_context` with elements:
#'   `run_id`, `study_id`, `environment`, `user`, `created_at`.
#'
#' @examples
#' ctx <- r4sub_run_context(study_id = "STUDY001", environment = "DEV")
#' ctx$run_id
#' ctx$study_id
#'
#' @importFrom rlang arg_match
#' @importFrom cli cli_alert_info
#' @export
r4sub_run_context <- function(study_id,
                              environment = c("DEV", "UAT", "PROD"),
                              user = NULL,
                              run_id = NULL,
                              timestamp = Sys.time()) {
  stopifnot(is.character(study_id), length(study_id) == 1L, nzchar(study_id))
  environment <- rlang::arg_match(environment)

  if (is.null(user)) {
    user <- Sys.info()[["user"]]
  }
  if (is.null(run_id)) {
    run_id <- generate_run_id(timestamp)
  }

  ctx <- list(
    run_id = run_id,
    study_id = study_id,
    environment = environment,
    user = user,
    created_at = timestamp
  )
  class(ctx) <- "r4sub_run_context"
  cli::cli_alert_info("Run context created: {.val {run_id}}")
  ctx
}

#' @export
print.r4sub_run_context <- function(x, ...) {
  cat("<r4sub_run_context>\n")
  cat("  run_id:      ", x$run_id, "\n")
  cat("  study_id:    ", x$study_id, "\n")
  cat("  environment: ", x$environment, "\n")
  cat("  user:        ", x$user, "\n")
  cat("  created_at:  ", format(x$created_at), "\n")
  invisible(x)
}
