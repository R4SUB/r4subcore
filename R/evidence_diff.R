#' Compare Two Evidence Tables
#'
#' Reports what changed between two evidence tables, for example the same
#' submission checked at two points in time. Rows are matched on a key (by
#' default the indicator applied to a specific asset) and classified as added,
#' removed, changed, or unchanged, with a direction that says whether each
#' change made the submission better or worse.
#'
#' When a key appears on more than one row within a table (an indicator that
#' emits several messages for the same asset, say), the rows are collapsed to
#' the worst one for that key before comparison: lowest result score first
#' (`fail` beats `warn` beats `pass`), then highest severity. This keeps the
#' diff at the indicator level, which is what a submission team acts on.
#'
#' @param old,new Valid evidence data.frames, as produced by [as_evidence()].
#'   `old` is the earlier state and `new` the later one.
#' @param key Character vector of columns that identify a comparable row.
#'   Default `c("indicator_id", "asset_id")`.
#' @param include_unchanged Logical. If `FALSE` (default) unchanged matched
#'   rows are dropped from the result.
#'
#' @return A data.frame with the key columns plus `indicator_name`,
#'   `indicator_domain`, `change` (`added`, `removed`, `changed`, or
#'   `unchanged`), `direction` (`improved`, `regressed`, or `neutral`),
#'   `result_from`, `result_to`, `severity_from`, `severity_to`,
#'   `metric_from`, and `metric_to`. Rows are ordered with the most actionable
#'   first: regressions, then new findings, then resolved findings, then
#'   improvements. The returned object carries a `"summary"` attribute with the
#'   count in each `change` category.
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
#' evidence_diff(old, new)
#'
#' @importFrom cli cli_abort
#' @export
evidence_diff <- function(old, new,
                          key = c("indicator_id", "asset_id"),
                          include_unchanged = FALSE) {
  validate_evidence(old)
  validate_evidence(new)
  if (!is.character(key) || length(key) < 1) {
    cli::cli_abort("{.arg key} must be a non-empty character vector.")
  }
  missing_old <- setdiff(key, names(old))
  missing_new <- setdiff(key, names(new))
  if (length(missing_old) || length(missing_new)) {
    cli::cli_abort(
      "Key column{?s} not found: {.field {unique(c(missing_old, missing_new))}}."
    )
  }

  old1 <- collapse_to_worst(old, key)
  new1 <- collapse_to_worst(new, key)

  old_keys <- old1$.key
  new_keys <- new1$.key
  all_keys <- union(old_keys, new_keys)

  if (length(all_keys) == 0) {
    empty <- stats::setNames(
      lapply(key, function(.) character(0)), key
    )
    out <- data.frame(
      c(empty, list(
        indicator_name = character(0), indicator_domain = character(0),
        change = character(0), direction = character(0),
        result_from = character(0), result_to = character(0),
        severity_from = character(0), severity_to = character(0),
        metric_from = double(0), metric_to = double(0)
      )),
      stringsAsFactors = FALSE
    )
    attr(out, "summary") <- table(factor(
      character(0), levels = c("changed", "added", "removed", "unchanged")
    ))
    return(out)
  }

  oi <- match(all_keys, old_keys)
  ni <- match(all_keys, new_keys)
  in_old <- !is.na(oi)
  in_new <- !is.na(ni)

  pick <- function(tbl, idx, col) {
    out <- rep(NA, length(idx))
    ok <- !is.na(idx)
    out[ok] <- tbl[[col]][idx[ok]]
    out
  }

  result_from   <- pick(old1, oi, "result")
  result_to     <- pick(new1, ni, "result")
  severity_from <- pick(old1, oi, "severity")
  severity_to   <- pick(new1, ni, "severity")
  metric_from   <- as.double(pick(old1, oi, "metric_value"))
  metric_to     <- as.double(pick(new1, ni, "metric_value"))

  # Identity for the human-facing labels: prefer the new table's values.
  ind_name   <- ifelse(in_new, pick(new1, ni, "indicator_name"),
                       pick(old1, oi, "indicator_name"))
  ind_domain <- ifelse(in_new, pick(new1, ni, "indicator_domain"),
                       pick(old1, oi, "indicator_domain"))

  change <- ifelse(!in_old, "added",
            ifelse(!in_new, "removed", "same-key"))
  differs <- diff_ne(result_from, result_to) |
             diff_ne(severity_from, severity_to) |
             diff_ne(metric_from, metric_to)
  change[change == "same-key"] <- ifelse(differs[change == "same-key"],
                                         "changed", "unchanged")

  direction <- mapply(
    diff_direction, change, result_from, result_to, severity_from, severity_to,
    USE.NAMES = FALSE
  )

  key_cols <- do.call(rbind, strsplit(all_keys, "\x01", fixed = TRUE))
  key_df <- as.data.frame(key_cols, stringsAsFactors = FALSE)
  names(key_df) <- key

  out <- cbind(
    key_df,
    data.frame(
      indicator_name   = as.character(ind_name),
      indicator_domain = as.character(ind_domain),
      change           = change,
      direction        = direction,
      result_from      = as.character(result_from),
      result_to        = as.character(result_to),
      severity_from    = as.character(severity_from),
      severity_to      = as.character(severity_to),
      metric_from      = metric_from,
      metric_to        = metric_to,
      stringsAsFactors = FALSE
    )
  )

  if (!include_unchanged) {
    out <- out[out$change != "unchanged", , drop = FALSE]
  }

  # Most actionable first.
  change_rank <- c(changed = 1L, added = 2L, removed = 3L, unchanged = 4L)
  dir_rank <- c(regressed = 1L, neutral = 2L, improved = 3L)
  ord <- order(change_rank[out$change], dir_rank[out$direction],
               out$indicator_id %||% out[[key[1]]])
  out <- out[ord, , drop = FALSE]
  rownames(out) <- NULL

  attr(out, "summary") <- table(factor(
    out$change, levels = c("changed", "added", "removed", "unchanged")
  ))
  out
}

# Collapse an evidence table to one row per key, keeping the worst row:
# lowest result score, then highest severity weight. Adds a `.key` column.
collapse_to_worst <- function(ev, key) {
  keyval <- do.call(paste, c(ev[key], sep = "\x01"))
  ev$.key <- keyval
  rs <- result_to_score(ev$result)
  rs[is.na(rs)] <- 2  # push `na` results below real pass/warn/fail
  sw <- severity_to_weight(ev$severity)
  # order so the worst row of each key comes first, then keep the first per key
  ord <- order(keyval, rs, -sw)
  ev <- ev[ord, , drop = FALSE]
  ev[!duplicated(ev$.key), , drop = FALSE]
}

# NA-aware inequality: TRUE when exactly one is NA, or both present and unequal.
diff_ne <- function(a, b) {
  (xor(is.na(a), is.na(b))) | (!is.na(a) & !is.na(b) & a != b)
}

# Direction of a single change, from the earlier to the later state.
diff_direction <- function(change, r_from, r_to, s_from, s_to) {
  score <- function(r) {
    v <- result_to_score(r)
    if (is.na(v)) NA_real_ else v
  }
  sev <- function(s) if (is.na(s)) NA_real_ else severity_to_weight(s)

  if (change == "unchanged") return("neutral")

  if (change == "added") {
    v <- score(r_to)
    if (is.na(v)) return("neutral")
    return(if (v >= 1) "improved" else "regressed")
  }
  if (change == "removed") {
    v <- score(r_from)
    if (is.na(v)) return("neutral")
    # A resolved problem is an improvement; losing a passing check is a regression.
    return(if (v >= 1) "regressed" else "improved")
  }

  # change == "changed": compare result quality first, severity as tiebreak.
  vf <- score(r_from); vt <- score(r_to)
  if (!is.na(vf) && !is.na(vt) && vt != vf) {
    return(if (vt > vf) "improved" else "regressed")
  }
  sf <- sev(s_from); st <- sev(s_to)
  if (!is.na(sf) && !is.na(st) && st != sf) {
    return(if (st < sf) "improved" else "regressed")
  }
  "neutral"
}

# Minimal null-coalescing helper (rlang's %||% is not imported here).
`%||%` <- function(x, y) if (is.null(x)) y else x
