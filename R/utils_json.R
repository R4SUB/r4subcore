#' Safely Serialize to JSON String
#'
#' Converts an R object to a valid JSON string. Returns `"{}"` on failure
#' or for `NULL`/empty inputs.
#'
#' @param x An R object to serialize.
#' @return A single character string containing valid JSON.
#'
#' @examples
#' json_safely(list(a = 1, b = "hello"))
#' json_safely(NULL)
#'
#' @importFrom jsonlite toJSON fromJSON
#' @export
json_safely <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("{}")
  }
  tryCatch(
    {
      out <- jsonlite::toJSON(x, auto_unbox = TRUE, na = "null")
      as.character(out)
    },
    error = function(e) {
      "{}"
    }
  )
}

#' Parse a JSON String Safely
#'
#' Attempts to parse a JSON string. Returns `NULL` on failure.
#'
#' @param x A character JSON string.
#' @return Parsed R object, or `NULL` on failure.
#' @noRd
parse_json_safely <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) {
    return(NULL)
  }
  tryCatch(
    jsonlite::fromJSON(x, simplifyVector = FALSE),
    error = function(e) NULL
  )
}
