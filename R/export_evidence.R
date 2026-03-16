#' Export Evidence Table to File
#'
#' Validates the evidence table then writes it to disk in the requested format.
#' Metadata attributes (`exported_at`, `r4subcore_version`, `nrow`) are
#' attached to the returned path value for traceability.
#'
#' @param evidence A valid evidence data.frame (as produced by [as_evidence()]).
#' @param file Character. Destination file path (including extension).
#' @param format Character. One of `"csv"`, `"rds"`, or `"json"`.
#'   Default: `"csv"`.
#'
#' @return Invisibly returns `file` with attributes:
#'   \describe{
#'     \item{exported_at}{POSIXct timestamp of export.}
#'     \item{r4subcore_version}{Package version string.}
#'     \item{nrow}{Number of rows written.}
#'   }
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
#' tmp_csv <- tempfile(fileext = ".csv")
#' out <- suppressMessages(export_evidence(ev, tmp_csv, format = "csv"))
#' file.exists(out)
#'
#' @importFrom jsonlite toJSON
#' @importFrom cli cli_alert_success cli_abort
#' @export
export_evidence <- function(evidence,
                            file,
                            format = c("csv", "rds", "json")) {
  format <- match.arg(format)
  stopifnot(is.character(file), length(file) == 1L, nzchar(file))

  validate_evidence(evidence)

  switch(
    format,
    csv  = utils::write.csv(evidence, file = file, row.names = FALSE),
    rds  = saveRDS(evidence, file = file),
    json = writeLines(
      jsonlite::toJSON(evidence, dataframe = "rows", auto_unbox = TRUE,
                       POSIXt = "string", na = "null"),
      con = file
    )
  )

  out <- file
  attr(out, "exported_at")       <- Sys.time()
  attr(out, "r4subcore_version") <- as.character(
    utils::packageVersion("r4subcore")
  )
  attr(out, "nrow") <- nrow(evidence)

  cli::cli_alert_success(
    "Evidence exported: {nrow(evidence)} row{?s} to {.file {file}} ({format})"
  )
  invisible(out)
}


#' Import Evidence Table from File
#'
#' Reads an evidence table that was previously saved by [export_evidence()],
#' then validates it against the evidence schema.
#'
#' @param file Character. Path to the file to read.
#' @param format Character. One of `"csv"`, `"rds"`, or `"json"`.
#'   Default: `"csv"`.
#'
#' @return A validated evidence data.frame conforming to the evidence schema.
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
#' tmp_rds <- tempfile(fileext = ".rds")
#' suppressMessages(export_evidence(ev, tmp_rds, format = "rds"))
#' ev2 <- suppressMessages(import_evidence(tmp_rds, format = "rds"))
#' nrow(ev2)
#'
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_alert_success cli_abort
#' @export
import_evidence <- function(file, format = c("csv", "rds", "json")) {
  format <- match.arg(format)
  stopifnot(is.character(file), length(file) == 1L)

  if (!file.exists(file)) {
    cli::cli_abort("File not found: {.file {file}}")
  }

  ev <- switch(
    format,
    csv  = utils::read.csv(file, stringsAsFactors = FALSE,
                           check.names = FALSE),
    rds  = readRDS(file),
    json = jsonlite::fromJSON(file, simplifyDataFrame = TRUE)
  )

  if (!is.data.frame(ev)) {
    cli::cli_abort(
      "Imported object is not a data.frame (got {.cls {class(ev)[1]}})"
    )
  }

  # Coerce created_at back to POSIXct (csv/json may read it as character)
  if ("created_at" %in% names(ev) && !inherits(ev$created_at, "POSIXct")) {
    ev$created_at <- as.POSIXct(ev$created_at, tz = "UTC")
  }

  # Coerce metric_value back to double (csv may read as integer)
  if ("metric_value" %in% names(ev) && !is.double(ev$metric_value)) {
    ev$metric_value <- as.double(ev$metric_value)
  }

  validate_evidence(ev)
  cli::cli_alert_success(
    "Evidence imported: {nrow(ev)} row{?s} from {.file {file}} ({format})"
  )
  ev
}
