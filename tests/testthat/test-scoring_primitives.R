test_that("severity_to_weight maps correctly", {
  expect_equal(severity_to_weight("info"), 0)
  expect_equal(severity_to_weight("low"), 0.25)
  expect_equal(severity_to_weight("medium"), 0.5)
  expect_equal(severity_to_weight("high"), 0.75)
  expect_equal(severity_to_weight("critical"), 1.0)
  expect_equal(
    severity_to_weight(c("low", "high")),
    c(0.25, 0.75)
  )
})

test_that("severity_to_weight rejects invalid values", {
  expect_error(severity_to_weight("bogus"), "Unknown severity")
})

test_that("result_to_score maps correctly", {
  expect_equal(result_to_score("pass"), 1.0)
  expect_equal(result_to_score("warn"), 0.5)
  expect_equal(result_to_score("fail"), 0.0)
  expect_true(is.na(result_to_score("na")))
})

test_that("normalize_01 works higher_better", {
  x <- c(10, 20, 30, 40, 50)
  out <- normalize_01(x)
  expect_equal(out, c(0, 0.25, 0.5, 0.75, 1))
})

test_that("normalize_01 works lower_better", {
  x <- c(10, 20, 30)
  out <- normalize_01(x, direction = "lower_better")
  expect_equal(out, c(1, 0.5, 0))
})

test_that("normalize_01 handles constant vector", {
  out <- normalize_01(c(5, 5, 5))
  expect_equal(out, c(0.5, 0.5, 0.5))
})

test_that("normalize_01 handles all-NA input", {
  out <- normalize_01(c(NA_real_, NA_real_))
  expect_true(all(is.na(out)))
  expect_equal(length(out), 2)
})

test_that("normalize_01 handles NAs", {
  out <- normalize_01(c(0, NA, 10))
  expect_equal(out[1], 0)
  expect_true(is.na(out[2]))
  expect_equal(out[3], 1)
})

test_that("result_to_score rejects unknown values", {
  expect_error(result_to_score("bogus"), "Unknown result")
})

test_that("aggregate_indicator_score multi-group by works", {
  ctx <- suppressMessages(r4sub_run_context("S1", "DEV"))
  ev <- suppressMessages(as_evidence(data.frame(
    asset_type = rep("validation", 4),
    asset_id = c("DS1", "DS1", "DS2", "DS2"),
    source_name = rep("test", 4),
    indicator_id = c("IND1", "IND1", "IND1", "IND1"),
    indicator_name = rep("Test", 4),
    indicator_domain = rep("quality", 4),
    severity = c("low", "high", "low", "medium"),
    result = c("pass", "fail", "pass", "pass"),
    stringsAsFactors = FALSE
  ), ctx = ctx))
  agg <- aggregate_indicator_score(ev, by = c("indicator_id", "asset_id"), method = "mean")
  expect_equal(nrow(agg), 2)
})

test_that("aggregate_indicator_score handles all-na results", {
  ctx <- suppressMessages(r4sub_run_context("S1", "DEV"))
  ev <- suppressMessages(as_evidence(data.frame(
    asset_type = rep("validation", 2),
    asset_id = rep("DS1", 2),
    source_name = rep("test", 2),
    indicator_id = rep("IND1", 2),
    indicator_name = rep("Test", 2),
    indicator_domain = rep("quality", 2),
    severity = c("low", "low"),
    result = c("na", "na"),
    stringsAsFactors = FALSE
  ), ctx = ctx))
  agg <- aggregate_indicator_score(ev, by = "indicator_id", method = "min")
  expect_true(is.na(agg$score))
})

test_that("aggregate_indicator_score mean method works", {
  ctx <- suppressMessages(r4sub_run_context("S1", "DEV"))
  ev <- suppressMessages(as_evidence(data.frame(
    asset_type = rep("validation", 3),
    asset_id = rep("DS1", 3),
    source_name = rep("test", 3),
    indicator_id = rep("IND1", 3),
    indicator_name = rep("Test", 3),
    indicator_domain = rep("quality", 3),
    severity = c("low", "high", "medium"),
    result = c("pass", "fail", "warn"),
    stringsAsFactors = FALSE
  ), ctx = ctx))

  agg <- aggregate_indicator_score(ev, by = "indicator_id", method = "mean")
  expect_equal(nrow(agg), 1)
  expect_equal(agg$n_evidence, 3)
  # mean of (1, 0, 0.5) = 0.5
  expect_equal(agg$score, 0.5)
})

test_that("aggregate_indicator_score weighted method works", {
  ctx <- suppressMessages(r4sub_run_context("S1", "DEV"))
  ev <- suppressMessages(as_evidence(data.frame(
    asset_type = rep("validation", 2),
    asset_id = rep("DS1", 2),
    source_name = rep("test", 2),
    indicator_id = rep("IND1", 2),
    indicator_name = rep("Test", 2),
    indicator_domain = rep("quality", 2),
    severity = c("low", "critical"),
    result = c("pass", "fail"),
    stringsAsFactors = FALSE
  ), ctx = ctx))

  agg <- aggregate_indicator_score(ev, by = "indicator_id", method = "weighted")
  expect_equal(nrow(agg), 1)
  # weighted.mean(c(1, 0), c(0.25, 1.0)) = 0.25/1.25 = 0.2
  expect_equal(agg$score, 0.2)
})

test_that("aggregate_indicator_score min method works", {
  ctx <- suppressMessages(r4sub_run_context("S1", "DEV"))
  ev <- suppressMessages(as_evidence(data.frame(
    asset_type = rep("validation", 2),
    asset_id = rep("DS1", 2),
    source_name = rep("test", 2),
    indicator_id = rep("IND1", 2),
    indicator_name = rep("Test", 2),
    indicator_domain = rep("quality", 2),
    severity = c("low", "medium"),
    result = c("pass", "warn"),
    stringsAsFactors = FALSE
  ), ctx = ctx))

  agg <- aggregate_indicator_score(ev, by = "indicator_id", method = "min")
  expect_equal(agg$score, 0.5)
})
