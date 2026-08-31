# Tests for evidence_diff (R/evidence_diff.R)

ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))

mk <- function(id, result, severity, asset = "ADSL", metric = NA_real_) {
  suppressMessages(as_evidence(
    data.frame(
      asset_type = "validation", asset_id = asset,
      source_name = "pinnacle21", indicator_id = id,
      indicator_name = id, indicator_domain = "quality",
      severity = severity, result = result, metric_value = metric,
      stringsAsFactors = FALSE
    ),
    ctx = ctx
  ))
}

test_that("added, removed, changed, and unchanged are classified", {
  old <- suppressMessages(bind_evidence(
    mk("A", "fail", "high"), mk("B", "pass", "low"), mk("D", "pass", "low")
  ))
  new <- suppressMessages(bind_evidence(
    mk("A", "pass", "high"),   # changed: fail -> pass (improved)
    mk("C", "fail", "medium"), # added: new failure (regressed)
    mk("D", "pass", "low")     # unchanged
  ))
  d <- evidence_diff(old, new)

  expect_s3_class(d, "data.frame")
  # unchanged dropped by default
  expect_false("D" %in% d$indicator_id)
  expect_equal(d$change[d$indicator_id == "A"], "changed")
  expect_equal(d$change[d$indicator_id == "B"], "removed")
  expect_equal(d$change[d$indicator_id == "C"], "added")
})

test_that("direction reflects better/worse transitions", {
  old <- suppressMessages(bind_evidence(mk("A", "fail", "high"), mk("B", "pass", "low")))
  new <- suppressMessages(bind_evidence(mk("A", "pass", "high"), mk("C", "fail", "high")))
  d <- evidence_diff(old, new)

  expect_equal(d$direction[d$indicator_id == "A"], "improved")  # fail -> pass
  expect_equal(d$direction[d$indicator_id == "B"], "regressed") # passing check dropped
  expect_equal(d$direction[d$indicator_id == "C"], "regressed") # new failure
})

test_that("removed passing check is a regression, removed failure is improvement", {
  old <- suppressMessages(bind_evidence(mk("P", "pass", "low"), mk("F", "fail", "high")))
  new <- suppressMessages(bind_evidence(mk("K", "pass", "low")))
  d <- evidence_diff(old, new)
  expect_equal(d$direction[d$indicator_id == "P"], "regressed") # lost coverage
  expect_equal(d$direction[d$indicator_id == "F"], "improved")  # problem resolved
})

test_that("severity change alone counts as changed", {
  old <- suppressMessages(mk("A", "fail", "medium"))
  new <- suppressMessages(mk("A", "fail", "high"))
  d <- evidence_diff(old, new)
  expect_equal(d$change, "changed")
  expect_equal(d$direction, "regressed") # severity got worse
})

test_that("include_unchanged returns the full picture", {
  old <- suppressMessages(mk("A", "pass", "low"))
  new <- suppressMessages(mk("A", "pass", "low"))
  d <- evidence_diff(old, new, include_unchanged = TRUE)
  expect_equal(nrow(d), 1L)
  expect_equal(d$change, "unchanged")
  expect_equal(d$direction, "neutral")
})

test_that("duplicate keys collapse to the worst row", {
  old <- suppressMessages(bind_evidence(mk("A", "pass", "low"), mk("A", "fail", "high")))
  new <- suppressMessages(mk("A", "pass", "low"))
  d <- evidence_diff(old, new)
  # old A collapses to the fail row, so pass now is an improvement
  expect_equal(nrow(d), 1L)
  expect_equal(d$change, "changed")
  expect_equal(d$result_from, "fail")
  expect_equal(d$direction, "improved")
})

test_that("summary attribute counts each change category", {
  old <- suppressMessages(bind_evidence(mk("A", "fail", "high"), mk("B", "pass", "low")))
  new <- suppressMessages(bind_evidence(mk("A", "pass", "high"), mk("C", "fail", "low")))
  d <- evidence_diff(old, new)
  s <- attr(d, "summary")
  expect_equal(as.integer(s[["changed"]]), 1L)
  expect_equal(as.integer(s[["added"]]), 1L)
  expect_equal(as.integer(s[["removed"]]), 1L)
})

test_that("asset is part of the default key", {
  old <- suppressMessages(mk("A", "fail", "high", asset = "ADSL"))
  new <- suppressMessages(mk("A", "fail", "high", asset = "ADAE"))
  d <- evidence_diff(old, new)
  # same indicator, different asset -> one removed, one added
  expect_setequal(d$change, c("removed", "added"))
})

test_that("two empty tables diff to an empty frame", {
  # as_evidence() does not build 0-row tables; subset a real one to 0 rows
  # while keeping the schema columns and types.
  empty <- mk("A", "pass", "low")[0, , drop = FALSE]
  d <- evidence_diff(empty, empty)
  expect_equal(nrow(d), 0L)
  expect_true(all(c("indicator_id", "asset_id", "change", "direction") %in% names(d)))
})
