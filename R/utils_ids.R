#' Generate a Stable Hash ID
#'
#' Creates a deterministic hash from one or more character inputs. Uses MD5
#' via base R's [digest-like approach][base] for a lightweight, dependency-free
#' implementation.
#'
#' @param ... Character values to hash together. Concatenated with `"|"`.
#' @param prefix Optional prefix prepended to the hash (e.g., `"RUN"`, `"IND"`).
#' @return A character string of the form `prefix-hexhash` or just `hexhash`.
#'
#' @examples
#' hash_id("ADSL", "rule_001")
#' hash_id("my_study", "2024-01-01", prefix = "RUN")
#'
#' @export
hash_id <- function(..., prefix = NULL) {
  parts <- paste(c(...), collapse = "|")
  # Use a temp file approach with md5sum for zero-dependency hashing

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeLines(parts, con = tmp)
  md5 <- unname(tools::md5sum(tmp))
  if (!is.null(prefix)) {
    paste0(prefix, "-", md5)
  } else {
    md5
  }
}

#' Generate a Run ID
#'
#' Internal helper to create a unique run identifier from timestamp + random
#' suffix.
#'
#' @param timestamp A POSIXct timestamp.
#' @return A character run ID.
#' @noRd
generate_run_id <- function(timestamp = Sys.time()) {
  ts_str <- format(timestamp, "%Y%m%d%H%M%S")
  suffix <- paste0(
    sample(c(letters, 0:9), 8, replace = TRUE),
    collapse = ""
  )
  paste0("R4S-", ts_str, "-", suffix)
}
