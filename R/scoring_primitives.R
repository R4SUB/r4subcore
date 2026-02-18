#' Map Severity to Numeric Weight
#'
#' Converts canonical severity labels to numeric penalty multipliers on
#' a 0--1 scale.
#'
#' @param severity Character vector of canonical severity values
#'   (`info`, `low`, `medium`, `high`, `critical`).
#' @return Numeric vector of weights.
#'
#' @details
#' Default mapping:
#' - `info`     = 0.00
#' - `low`      = 0.25
#' - `medium`   = 0.50
#' - `high`     = 0.75
#' - `critical` = 1.00
#'
#' @examples
#' severity_to_weight(c("low", "high", "critical"))
#'
#' @export
severity_to_weight <- function(severity) {
  map <- c(
    info     = 0.00,
    low      = 0.25,
    medium   = 0.50,
    high     = 0.75,
    critical = 1.00
  )
  out <- unname(map[severity])
  if (any(is.na(out) & !is.na(severity))) {
    bad <- unique(severity[is.na(out) & !is.na(severity)])
    cli::cli_abort("Unknown severity value{?s}: {.val {bad}}")
  }
  out
}

#' Map Result to Numeric Score
#'
#' Converts canonical result labels to numeric scores.
#'
#' @param result Character vector of canonical result values
#'   (`pass`, `fail`, `warn`, `na`).
#' @return Numeric vector: pass=1, warn=0.5, fail=0, na=NA.
#'
#' @examples
#' result_to_score(c("pass", "fail", "warn", "na"))
#'
#' @export
result_to_score <- function(result) {
  map <- c(
    pass = 1.0,
    warn = 0.5,
    fail = 0.0,
    na   = NA_real_
  )
  out <- unname(map[result])
  unknown <- is.na(out) & !is.na(result) & result != "na"
  if (any(unknown)) {
    bad <- unique(result[unknown])
    cli::cli_abort("Unknown result value{?s}: {.val {bad}}")
  }
  out
}

#' Normalize to 0--1 Range
#'
#' Applies min-max normalization to a numeric vector, optionally clamping
#' values to \[0, 1\].
#'
#' @param x Numeric vector.
#' @param direction Character. `"higher_better"` (default) maps max to 1;
#'   `"lower_better"` maps min to 1.
#' @param clamp Logical. If `TRUE`, clamp output to \[0, 1\].
#' @return Numeric vector normalized to 0--1.
#'
#' @examples
#' normalize_01(c(10, 20, 30, 40, 50))
#' normalize_01(c(10, 20, 30), direction = "lower_better")
#'
#' @importFrom rlang arg_match
#' @export
normalize_01 <- function(x, direction = c("higher_better", "lower_better"),
                         clamp = TRUE) {
  direction <- rlang::arg_match(direction)
  stopifnot(is.numeric(x))

  x_clean <- x[!is.na(x)]
  if (length(x_clean) == 0) return(rep(NA_real_, length(x)))

  rng <- range(x_clean)
  span <- rng[2] - rng[1]

  if (span == 0) {
    out <- rep(0.5, length(x))
    out[is.na(x)] <- NA_real_
    return(out)
  }

  out <- (x - rng[1]) / span
  if (direction == "lower_better") {
    out <- 1 - out
  }
  if (clamp) {
    out <- pmin(pmax(out, 0), 1)
  }
  out
}

#' Aggregate Indicator Scores
#'
#' Computes summary scores from an evidence table, grouped by one or more
#' columns.
#'
#' @param ev A valid evidence data.frame.
#' @param by Character vector of column names to group by.
#'   Default: `c("indicator_id")`.
#' @param method Aggregation method: `"mean"`, `"min"`, or `"weighted"`.
#'   The `"weighted"` method uses [severity_to_weight()] and
#'   [result_to_score()].
#' @return A data.frame with grouping columns plus `score` (0--1) and
#'   `n_evidence` (count of rows).
#'
#' @examples
#' ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
#' ev <- suppressMessages(as_evidence(
#'   data.frame(
#'     asset_type = rep("validation", 3), asset_id = rep("ADSL", 3),
#'     source_name = rep("pinnacle21", 3),
#'     indicator_id = c("SD0001", "SD0001", "SD0002"),
#'     indicator_name = c("SD0001", "SD0001", "SD0002"),
#'     indicator_domain = rep("quality", 3),
#'     severity = c("high", "medium", "low"),
#'     result = c("fail", "warn", "pass"),
#'     stringsAsFactors = FALSE
#'   ),
#'   ctx = ctx
#' ))
#' aggregate_indicator_score(ev, by = "indicator_id", method = "weighted")
#'
#' @importFrom stats weighted.mean
#' @importFrom rlang arg_match
#' @export
aggregate_indicator_score <- function(ev,
                                      by = "indicator_id",
                                      method = c("mean", "min", "weighted")) {
  method <- rlang::arg_match(method)
  validate_evidence(ev)
  stopifnot(all(by %in% names(ev)))

  # Build grouping factor
  if (length(by) == 1) {
    grp <- ev[[by]]
  } else {
    grp <- interaction(ev[, by, drop = FALSE], drop = TRUE, sep = "|")
  }

  result_scores <- result_to_score(ev$result)
  sev_weights <- severity_to_weight(ev$severity)

  groups <- split(seq_len(nrow(ev)), grp)
  out_list <- lapply(names(groups), function(g) {
    idx <- groups[[g]]
    rs <- result_scores[idx]
    sw <- sev_weights[idx]

    score <- switch(
      method,
      mean = if (all(is.na(rs))) NA_real_ else mean(rs, na.rm = TRUE),
      min  = if (all(is.na(rs))) NA_real_ else min(rs, na.rm = TRUE),
      weighted = {
        valid <- !is.na(rs)
        if (!any(valid)) {
          NA_real_
        } else {
          # Weight = severity weight; score = result score

          # Higher severity => bigger penalty, so weighted score reflects that
          w <- sw[valid]
          s <- rs[valid]
          if (sum(w) == 0) {
            mean(s)
          } else {
            stats::weighted.mean(s, w)
          }
        }
      }
    )

    # Extract group values
    first_row <- ev[idx[1], by, drop = FALSE]
    cbind(first_row, data.frame(score = score, n_evidence = length(idx),
                                stringsAsFactors = FALSE))
  })

  result <- do.call(rbind, out_list)
  rownames(result) <- NULL
  result
}
