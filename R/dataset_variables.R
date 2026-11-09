#' List the Variables in Each Dataset
#'
#' Builds the `(dataset, variable)` table that [check_define_consistency()]
#' compares against Define-XML. It reads only the names present in each dataset;
#' it does not open the file for any other purpose, and it emits no scoring or
#' evidence of its own. The companion on the Define-XML side is
#' [define_variables()].
#'
#' The input can be:
#' * a single data frame, with its dataset name given through `dataset`;
#' * a named list of data frames, where each name is used as the dataset name;
#' * a path to a directory of files, one data frame each, where the file name
#'   (without extension) is used as the dataset name. `.rds` files are read with
#'   base R. SAS transport (`.xpt`) files are read when the optional `haven`
#'   package is installed; without it they are skipped.
#'
#' @param x A data frame, a named list of data frames, or a path to a directory
#'   of dataset files.
#' @param dataset For a single data frame, the dataset name to record. Ignored,
#'   and not required, for the list and directory forms.
#'
#' @return A [tibble][tibble::tibble] with character columns `dataset` and
#'   `variable`, one row per variable in each dataset.
#'
#' @seealso [check_define_consistency()], [define_variables()].
#'
#' @examples
#' # A named list of data frames: the names become the dataset names.
#' dataset_variables(list(
#'   ADSL = data.frame(USUBJID = character(), AGE = numeric()),
#'   ADAE = data.frame(USUBJID = character(), AEDECOD = character())
#' ))
#'
#' # A single data frame needs an explicit dataset name.
#' dataset_variables(data.frame(USUBJID = 1, SEX = "F"), dataset = "ADSL")
#'
#' # A directory of .rds files, one data frame each.
#' dir <- tempfile("datasets")
#' dir.create(dir)
#' saveRDS(data.frame(USUBJID = 1, AGE = 40), file.path(dir, "ADSL.rds"))
#' saveRDS(data.frame(USUBJID = 1, AEDECOD = "X"), file.path(dir, "ADAE.rds"))
#' dataset_variables(dir)
#'
#' # Feed the result straight into check_define_consistency().
#' ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
#' define <- data.frame(dataset = "ADSL", variable = c("USUBJID", "AGE"))
#' data <- dataset_variables(list(ADSL = data.frame(USUBJID = 1, AGE = 40)))
#' suppressMessages(check_define_consistency(define, data, ctx))[, c("asset_id", "result")]
#'
#' @importFrom cli cli_abort
#' @importFrom tibble tibble
#' @export
dataset_variables <- function(x, dataset = NULL) {
  # A single data frame: the caller names the dataset.
  if (is.data.frame(x)) {
    if (is.null(dataset)) {
      cli::cli_abort(c(
        "A single data frame needs a {.arg dataset} name.",
        i = "Pass {.arg dataset}, or pass a named list of data frames."
      ))
    }
    if (!is.character(dataset) || length(dataset) != 1L || is.na(dataset)) {
      cli::cli_abort("{.arg dataset} must be a single, non-missing string.")
    }
    return(dsv_from_df(x, dataset))
  }

  # A directory of dataset files.
  if (is.character(x) && length(x) == 1L && !is.na(x) && dir.exists(x)) {
    return(dsv_from_dir(x))
  }

  # A named list of data frames.
  if (is.list(x)) {
    return(dsv_from_list(x))
  }

  cli::cli_abort(
    "{.arg x} must be a data frame, a named list of data frames, or a directory path."
  )
}

# One (dataset, variable) row per column name of a data frame.
dsv_from_df <- function(df, ds_name) {
  tibble::tibble(
    dataset  = rep(as.character(ds_name), length.out = ncol(df)),
    variable = as.character(names(df))
  )
}

dsv_empty <- function() {
  tibble::tibble(dataset = character(0), variable = character(0))
}

dsv_bind <- function(parts) {
  if (length(parts) == 0L) return(dsv_empty())
  do.call(rbind, parts)
}

dsv_from_list <- function(x) {
  nms <- names(x)
  if (is.null(nms) || anyNA(nms) || any(!nzchar(nms))) {
    cli::cli_abort(
      "A list input must be fully named; each name is used as the dataset name."
    )
  }
  is_df <- vapply(x, is.data.frame, logical(1))
  if (!all(is_df)) {
    bad <- nms[!is_df]
    cli::cli_abort("Every list element must be a data frame; {.val {bad}} {?is/are} not.")
  }
  dsv_bind(Map(dsv_from_df, x, nms))
}

# File extension (lower-case, empty when there is none) and name without it.
dsv_ext <- function(paths) {
  b <- basename(paths)
  ext <- rep("", length(b))
  has <- grepl("\\.[^.]+$", b)
  ext[has] <- tolower(sub(".*\\.", "", b[has]))
  ext
}
dsv_stem <- function(paths) sub("\\.[^.]*$", "", basename(paths))

dsv_read_file <- function(file, ext) {
  switch(ext,
    rds = readRDS(file),
    xpt = haven::read_xpt(file),
    cli::cli_abort("Unsupported dataset file: {.file {file}}.")
  )
}

dsv_from_dir <- function(dir) {
  files <- list.files(dir, full.names = TRUE)
  ext   <- dsv_ext(files)

  supported <- ext == "rds"
  if (requireNamespace("haven", quietly = TRUE)) {
    supported <- supported | ext == "xpt"
  }
  files <- files[supported]
  ext   <- ext[supported]
  if (length(files) == 0L) return(dsv_empty())

  ord   <- order(basename(files))
  files <- files[ord]
  ext   <- ext[ord]

  parts <- Map(function(f, e) {
    df <- dsv_read_file(f, e)
    if (!is.data.frame(df)) {
      cli::cli_abort("{.file {f}} does not contain a data frame.")
    }
    dsv_from_df(df, dsv_stem(f))
  }, files, ext)

  dsv_bind(parts)
}
