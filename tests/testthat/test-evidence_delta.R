# Tests for evidence_delta / evidence_diff_summary (R/evidence_delta.R)

ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))

mk <- function(id, result, severity, asset = "ADSL") {
  suppressMessages(as_evidence(
    data.frame(
      asset_type = "validation", asset_id = asset,
      source_name = "pinnacle21", indicator_id = id,
      indicator_name = id, indicator_domain = "quality",
      severity = severity, result = result,
      stringsAsFactors = FALSE
    ),
    ctx = ctx
  ))
}

test_that("evidence_delta appends a numeric delta without altering the diff", {
  old <- suppressMessages(bind_evidence(mk("A", "fail", "high"), mk("B", "pass", "low")))
  new <- suppressMessages(bind_evidence(mk("A", "pass", "high"), mk("C", "fail", "medium")))
  d <- evidence_diff(old, new)
  e <- evidence_delta(d)

  expect_s3_class(e, "data.frame")
  expect_true("delta" %in% names(e))
  expect_type(e$delta, "double")
  expect_equal(nrow(e), nrow(d))
  # existing columns and order untouched
  expect_equal(e[names(d)], d, ignore_attr = TRUE)
  # summary attribute preserved
  expect_identical(attr(e, "summary"), attr(d, "summary"))
})

test_that("signed delta: fixing a high failure is positive, adding one is negative", {
  old <- suppressMessages(mk("A", "fail", "high"))
  new <- suppressMessages(mk("A", "pass", "high"))
  e <- evidence_delta(evidence_diff(old, new))
  # penalty_from = (1-0)*0.75 = 0.75, penalty_to = 0 -> delta = +0.75
  expect_equal(e$delta[e$indicator_id == "A"], 0.75)

  # a brand-new medium failure adds penalty 0.5 -> delta = -0.5
  old2 <- suppressMessages(mk("A", "pass", "low"))
  new2 <- suppressMessages(bind_evidence(mk("A", "pass", "low"), mk("C", "fail", "medium")))
  e2 <- evidence_delta(evidence_diff(old2, new2))
  expect_equal(e2$delta[e2$indicator_id == "C"], -0.5)
})

test_that("passing-check coverage changes score zero even when direction moves", {
  # dropping a passing check: direction regressed, but no penalty load changes
  old <- suppressMessages(bind_evidence(mk("P", "pass", "low"), mk("K", "pass", "low")))
  new <- suppressMessages(mk("K", "pass", "low"))
  e <- evidence_delta(evidence_diff(old, new))
  expect_equal(e$direction[e$indicator_id == "P"], "regressed")
  expect_equal(e$delta[e$indicator_id == "P"], 0)
})

test_that("severity worsening on a failure is a negative delta", {
  old <- suppressMessages(mk("A", "fail", "medium"))
  new <- suppressMessages(mk("A", "fail", "high"))
  e <- evidence_delta(evidence_diff(old, new))
  # penalty_from = 0.5, penalty_to = 0.75 -> delta = -0.25
  expect_equal(e$delta, -0.25)
})

test_that("evidence_delta rejects things that are not an evidence_diff", {
  expect_error(evidence_delta(42), "data.frame")
  expect_error(evidence_delta(data.frame(x = 1)), "evidence_diff")
})

test_that("summary is a tidy metric/value frame with expected rows", {
  old <- suppressMessages(bind_evidence(mk("A", "fail", "high"), mk("B", "pass", "low")))
  new <- suppressMessages(bind_evidence(mk("A", "pass", "high"), mk("C", "fail", "medium")))
  s <- evidence_diff_summary(evidence_diff(old, new))

  expect_s3_class(s, "data.frame")
  expect_identical(names(s), c("metric", "value"))
  expect_type(s$value, "double")
  expected_metrics <- c(
    "n_rows", "n_added", "n_removed", "n_changed", "n_unchanged",
    "n_improved", "n_regressed", "n_neutral", "net_delta"
  )
  expect_identical(s$metric, expected_metrics)

  val <- stats::setNames(s$value, s$metric)
  # A changed, B removed, C added
  expect_equal(val[["n_changed"]], 1)
  expect_equal(val[["n_removed"]], 1)
  expect_equal(val[["n_added"]], 1)
  expect_equal(val[["n_rows"]], 3)
  # net = +0.75 (A fixed) + 0 (B passing dropped) - 0.5 (C new medium fail)
  expect_equal(val[["net_delta"]], 0.25)
})

test_that("counts cover every change and direction category", {
  old <- suppressMessages(bind_evidence(mk("A", "fail", "high"), mk("B", "pass", "low")))
  new <- suppressMessages(bind_evidence(mk("A", "pass", "high"), mk("B", "pass", "low")))
  s <- evidence_diff_summary(evidence_diff(old, new, include_unchanged = TRUE))
  val <- stats::setNames(s$value, s$metric)
  expect_equal(val[["n_changed"]], 1)   # A
  expect_equal(val[["n_unchanged"]], 1) # B
  expect_equal(val[["n_improved"]], 1)  # A fail -> pass
  expect_equal(val[["n_neutral"]], 1)   # B unchanged
  expect_equal(val[["n_rows"]], 2)
})

test_that("summary carries the diagnostic (not SCI) note", {
  old <- suppressMessages(mk("A", "fail", "high"))
  new <- suppressMessages(mk("A", "pass", "high"))
  s <- evidence_diff_summary(evidence_diff(old, new))
  note <- attr(s, "net_delta_note")
  expect_true(is.character(note) && grepl("SCI", note))
})

test_that("an empty diff summarises to all-zero counts", {
  empty <- mk("A", "pass", "low")[0, , drop = FALSE]
  d <- evidence_diff(empty, empty)
  s <- evidence_diff_summary(d)
  val <- stats::setNames(s$value, s$metric)
  expect_equal(val[["n_rows"]], 0)
  expect_equal(val[["net_delta"]], 0)
})
