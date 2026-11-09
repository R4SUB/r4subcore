#' Signed Per-Row Score Delta for an Evidence Diff
#'
#' Scores each row of an [evidence_diff()] result on a signed scale, so you can
#' see not just *that* a finding moved but *how much* weight that movement
#' carried. The score reuses the package's existing scoring primitives,
#' [result_to_score()] and [severity_to_weight()]; it invents no new weights.
#'
#' Each state (the `from` side and the `to` side of a row) is reduced to a
#' weighted penalty, `(1 - result_to_score(result)) * severity_to_weight(severity)`.
#' A passing check carries no penalty; a failing, high-severity check carries
#' the most. A row's signed delta is `penalty_from - penalty_to`, so a
#' **positive** delta means the weighted penalty fell (the submission got
#' better at that row) and a **negative** delta means it rose. States that are
#' absent (the missing side of an added or removed row) or `na` contribute no
#' penalty, so an added finding shows a negative delta and a resolved finding a
#' positive one.
#'
#' Note that the delta is penalty-based: adding or dropping a *passing* check
#' leaves the penalty load unchanged and so scores `0`, even though
#' [evidence_diff()] still labels it `improved` (more coverage) or `regressed`
#' (lost coverage) in the `direction` column. The two views are complementary:
#' `direction` is categorical and coverage-aware, the delta is a magnitude.
#'
#' @param diff A data.frame produced by [evidence_diff()] (it must carry the
#'   `change`, `direction`, `result_from`, `result_to`, `severity_from`, and
#'   `severity_to` columns).
#'
#' @return The input `diff`, unchanged in row order and contents, with one new
#'   numeric column `delta` appended: the signed per-row penalty movement
#'   described above. Any `"summary"` attribute on `diff` is preserved.
#'
#' @seealso [evidence_diff()] for the diff this consumes and
#'   [evidence_diff_summary()] for the tidy rollup built on top of it.
#'
#' @examples
#' ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
#' mk <- function(id, result, severity) {
#'   suppressMessages(as_evidence(
#'     data.frame(
#'       asset_type = "validation", asset_id = "ADSL",
#'       source_name = "pinnacle21", indicator_id = id,
#'       indicator_name = id, indicator_domain = "quality",
#'       severity = severity, result = result,
#'       stringsAsFactors = FALSE
#'     ),
#'     ctx = ctx
#'   ))
#' }
#' old <- suppressMessages(bind_evidence(mk("A", "fail", "high"), mk("B", "pass", "low")))
#' new <- suppressMessages(bind_evidence(mk("A", "pass", "high"), mk("C", "fail", "medium")))
#' d <- evidence_diff(old, new)
#' evidence_delta(d)[, c("indicator_id", "change", "direction", "delta")]
#'
#' @importFrom cli cli_abort
#' @export
evidence_delta <- function(diff) {
  if (!is.data.frame(diff)) {
    cli::cli_abort("{.arg diff} must be a data.frame produced by {.fn evidence_diff}.")
  }
  req <- c("change", "direction", "result_from", "result_to",
           "severity_from", "severity_to")
  missing <- setdiff(req, names(diff))
  if (length(missing)) {
    cli::cli_abort(c(
      "{.arg diff} does not look like an {.fn evidence_diff} result.",
      "x" = "Missing column{?s}: {.field {missing}}."
    ))
  }

  keep <- attr(diff, "summary")
  diff$delta <- delta_row(
    diff$result_from, diff$result_to, diff$severity_from, diff$severity_to
  )
  if (!is.null(keep)) attr(diff, "summary") <- keep
  diff
}

#' Tidy Summary of an Evidence Diff
#'
#' Rolls an [evidence_diff()] result up into a small, tidy data.frame: how many
#' rows fell into each `change` category, how many into each `direction`, and a
#' single net-movement figure.
#'
#' The net-movement figure, `net_delta`, is the sum of the signed per-row deltas
#' from [evidence_delta()] (weighted penalty on the `from` side minus the `to`
#' side). A positive `net_delta` means the submission's weighted penalty load
#' fell overall; a negative one means it rose.
#'
#' `net_delta` is a **diagnostic rollup only**. It is a convenient one-number
#' read on which way a diff moved, not a calibrated readiness score, and it is
#' **not** the Submission Confidence Index (SCI). Do not report or compare it as
#' an SCI value; use the ecosystem's SCI machinery for that.
#'
#' @param diff A data.frame produced by [evidence_diff()].
#'
#' @return A tidy data.frame with one row per metric and two columns, `metric`
#'   (character) and `value` (numeric). The metrics are `n_rows`; the change
#'   counts `n_added`, `n_removed`, `n_changed`, `n_unchanged`; the direction
#'   counts `n_improved`, `n_regressed`, `n_neutral`; and `net_delta`. The
#'   returned object carries a `"net_delta_note"` attribute restating that
#'   `net_delta` is a diagnostic, not the SCI.
#'
#' @seealso [evidence_diff()] and [evidence_delta()].
#'
#' @examples
#' ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
#' mk <- function(id, result, severity) {
#'   suppressMessages(as_evidence(
#'     data.frame(
#'       asset_type = "validation", asset_id = "ADSL",
#'       source_name = "pinnacle21", indicator_id = id,
#'       indicator_name = id, indicator_domain = "quality",
#'       severity = severity, result = result,
#'       stringsAsFactors = FALSE
#'     ),
#'     ctx = ctx
#'   ))
#' }
#' old <- suppressMessages(bind_evidence(mk("A", "fail", "high"), mk("B", "pass", "low")))
#' new <- suppressMessages(bind_evidence(mk("A", "pass", "high"), mk("C", "fail", "medium")))
#' evidence_diff_summary(evidence_diff(old, new))
#'
#' @importFrom cli cli_abort
#' @export
evidence_diff_summary <- function(diff) {
  d <- evidence_delta(diff)

  change_levels <- c("added", "removed", "changed", "unchanged")
  dir_levels    <- c("improved", "regressed", "neutral")
  cc <- table(factor(d$change, levels = change_levels))
  dc <- table(factor(d$direction, levels = dir_levels))
  net <- sum(d$delta, na.rm = TRUE)

  out <- data.frame(
    metric = c(
      "n_rows",
      paste0("n_", change_levels),
      paste0("n_", dir_levels),
      "net_delta"
    ),
    value = c(
      as.numeric(nrow(d)),
      as.numeric(cc[change_levels]),
      as.numeric(dc[dir_levels]),
      net
    ),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  attr(out, "net_delta_note") <- paste(
    "net_delta is a diagnostic rollup of signed per-row penalty movement,",
    "not the official Submission Confidence Index (SCI)."
  )
  out
}

# Signed per-row delta: weighted penalty on the `from` side minus the `to`
# side. Positive means the penalty fell (improvement).
delta_row <- function(result_from, result_to, severity_from, severity_to) {
  state_penalty(result_from, severity_from) -
    state_penalty(result_to, severity_to)
}

# Weighted penalty for a single state, built only from the existing primitives
# result_to_score() and severity_to_weight(). Missing or `na` states carry no
# penalty.
state_penalty <- function(result, severity) {
  rs <- result_to_score(result)
  sw <- severity_to_weight(severity)
  p <- (1 - rs) * sw
  p[is.na(p)] <- 0
  p
}
