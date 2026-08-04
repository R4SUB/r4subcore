#' Read a Pinnacle 21 Report from Disk
#'
#' Loads a Pinnacle 21 validation report (the export most teams already produce
#' for every submission) into a data.frame, ready for [p21_to_evidence()]. CSV
#' exports are read with base R. Excel exports are read with the `readxl`
#' package when it is installed; if it is not, export the report as CSV instead.
#'
#' This function only reads the file; it does not interpret the columns. Pass the
#' result to [p21_to_evidence()], which detects the rule, message, severity,
#' dataset, variable, and result columns case-insensitively.
#'
#' @param path Path to the report file.
#' @param format One of `"auto"` (infer from the file extension), `"csv"`, or
#'   `"xlsx"`. Default `"auto"`.
#' @param sheet For Excel reports, the sheet to read. When `NULL` (the default),
#'   a sheet whose name looks like the findings list (matching "detail", "issue",
#'   or "finding") is used, otherwise the first sheet.
#'
#' @return A data.frame of the report rows with the original column names.
#'
#' @seealso [p21_to_evidence()] to convert the result into evidence.
#'
#' @examples
#' # Simulate a small CSV export and read it back.
#' p21 <- data.frame(
#'   "Rule ID" = c("SD0001", "SD0002"),
#'   Message   = c("Missing variable label", "Invalid format"),
#'   Severity  = c("Error", "Warning"),
#'   Dataset   = c("ADSL", "ADAE"),
#'   Variable  = c("AGEU", "AESEQ"),
#'   Status    = c("Failed", "Warning"),
#'   check.names = FALSE
#' )
#' csv <- tempfile(fileext = ".csv")
#' utils::write.csv(p21, csv, row.names = FALSE)
#' report <- read_p21_report(csv)
#' report
#'
#' @importFrom cli cli_alert_info cli_abort
#' @export
read_p21_report <- function(path, format = c("auto", "csv", "xlsx"),
                            sheet = NULL) {
  stopifnot(is.character(path), length(path) == 1L, nzchar(path))
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.file {path}}")
  }
  format <- match.arg(format)

  if (format == "auto") {
    ext <- tolower(sub(".*\\.", "", basename(path)))
    format <- switch(
      ext,
      "csv"  = "csv",
      "txt"  = "csv",
      "xlsx" = "xlsx",
      "xls"  = "xlsx",
      cli::cli_abort(c(
        "Cannot infer the format from extension {.val {ext}}.",
        "i" = "Pass {.arg format} explicitly as {.val csv} or {.val xlsx}."
      ))
    )
  }

  if (format == "csv") {
    df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      cli::cli_abort(c(
        "Reading an Excel report needs the {.pkg readxl} package.",
        "i" = "Install {.pkg readxl}, or export the report as CSV."
      ))
    }
    sheets <- readxl::excel_sheets(path)
    target <- if (!is.null(sheet)) sheet else pick_p21_sheet(sheets)
    df <- as.data.frame(
      readxl::read_excel(path, sheet = target),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  cli::cli_alert_info("Read {nrow(df)} row{?s} from {.file {path}}")
  df
}

# Choose the most likely findings sheet from an Excel report. Internal.
pick_p21_sheet <- function(sheets) {
  hit <- grep("detail|issue|finding", sheets, ignore.case = TRUE, value = TRUE)
  if (length(hit) > 0L) hit[1] else sheets[1]
}
