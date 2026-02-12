test_that("evidence_schema returns expected structure", {
  s <- evidence_schema()
  expect_type(s, "list")
  expect_true("run_id" %in% names(s))
  expect_true("severity" %in% names(s))
  expect_true("created_at" %in% names(s))
  expect_equal(s$severity$type, "character")
  expect_true("critical" %in% s$severity$allowed)
})

test_that("validate_evidence catches missing required columns", {
  bad_df <- data.frame(x = 1)
  expect_error(validate_evidence(bad_df), "missing required column")
})

test_that("validate_evidence catches wrong column type", {
  ctx <- suppressMessages(r4sub_run_context("S1", "DEV"))
  df <- data.frame(
    run_id = ctx$run_id,
    study_id = "S1",
    asset_type = "validation",
    asset_id = "ADSL",
    source_name = "test",
    source_version = NA_character_,
    indicator_id = "IND001",
    indicator_name = "Test",
    indicator_domain = "quality",
    severity = "high",
    result = "fail",
    metric_value = "not_a_number",
    metric_unit = NA_character_,
    message = NA_character_,
    location = NA_character_,
    evidence_payload = "{}",
    created_at = Sys.time(),
    stringsAsFactors = FALSE
  )
  expect_error(validate_evidence(df), "must be")
})

test_that("validate_evidence catches NA in non-nullable column", {
  ctx <- suppressMessages(r4sub_run_context("S1", "DEV"))
  df <- data.frame(
    run_id = ctx$run_id,
    study_id = "S1",
    asset_type = "validation",
    asset_id = NA_character_,
    source_name = "test",
    source_version = NA_character_,
    indicator_id = "IND001",
    indicator_name = "Test",
    indicator_domain = "quality",
    severity = "high",
    result = "fail",
    metric_value = NA_real_,
    metric_unit = NA_character_,
    message = NA_character_,
    location = NA_character_,
    evidence_payload = "{}",
    created_at = Sys.time(),
    stringsAsFactors = FALSE
  )
  expect_error(validate_evidence(df), "must not contain NA")
})

test_that("validate_evidence catches invalid asset_type", {
  ctx <- suppressMessages(r4sub_run_context("S1", "DEV"))
  df <- data.frame(
    run_id = ctx$run_id,
    study_id = "S1",
    asset_type = "INVALID_TYPE",
    asset_id = "ADSL",
    source_name = "test",
    source_version = NA_character_,
    indicator_id = "IND001",
    indicator_name = "Test",
    indicator_domain = "quality",
    severity = "high",
    result = "fail",
    metric_value = NA_real_,
    metric_unit = NA_character_,
    message = NA_character_,
    location = NA_character_,
    evidence_payload = "{}",
    created_at = Sys.time(),
    stringsAsFactors = FALSE
  )
  expect_error(validate_evidence(df), "invalid value")
})

test_that("validate_evidence catches invalid severity values", {
  ctx <- suppressMessages(r4sub_run_context("S1", "DEV"))
  df <- data.frame(
    run_id = ctx$run_id,
    study_id = "S1",
    asset_type = "validation",
    asset_id = "ADSL",
    source_name = "test",
    source_version = NA_character_,
    indicator_id = "IND001",
    indicator_name = "Test",
    indicator_domain = "quality",
    severity = "BOGUS",
    result = "pass",
    metric_value = NA_real_,
    metric_unit = NA_character_,
    message = NA_character_,
    location = NA_character_,
    evidence_payload = "{}",
    created_at = Sys.time(),
    stringsAsFactors = FALSE
  )
  expect_error(validate_evidence(df), "invalid value")
})

test_that("validate_evidence passes for valid data", {
  ctx <- suppressMessages(r4sub_run_context("S1", "DEV"))
  df <- data.frame(
    run_id = ctx$run_id,
    study_id = "S1",
    asset_type = "validation",
    asset_id = "ADSL",
    source_name = "test",
    source_version = NA_character_,
    indicator_id = "IND001",
    indicator_name = "Test",
    indicator_domain = "quality",
    severity = "high",
    result = "fail",
    metric_value = NA_real_,
    metric_unit = NA_character_,
    message = "test message",
    location = "ADSL:AGE",
    evidence_payload = "{}",
    created_at = Sys.time(),
    stringsAsFactors = FALSE
  )
  expect_true(validate_evidence(df))
})

test_that("as_evidence fills context fields and validates", {
  ctx <- suppressMessages(r4sub_run_context("MYSTUDY", "DEV"))
  df <- data.frame(
    asset_type = "validation",
    asset_id = "ADSL",
    source_name = "pinnacle21",
    indicator_id = "P21-001",
    indicator_name = "Missing var",
    indicator_domain = "quality",
    severity = "high",
    result = "fail",
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(as_evidence(df, ctx = ctx))
  expect_equal(ev$run_id, ctx$run_id)
  expect_equal(ev$study_id, "MYSTUDY")
  expect_true(inherits(ev$created_at, "POSIXct"))
  expect_equal(ev$evidence_payload, "{}")
})

test_that("as_evidence uses dot arguments to fill columns", {
  ctx <- suppressMessages(r4sub_run_context("S1", "DEV"))
  df <- data.frame(
    asset_id = "ADSL",
    source_name = "test",
    indicator_id = "IND1",
    indicator_name = "Test",
    indicator_domain = "quality",
    severity = "low",
    result = "pass",
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(as_evidence(df, ctx = ctx, asset_type = "validation"))
  expect_equal(ev$asset_type, "validation")
})

test_that("bind_evidence rejects invalid frame", {
  ctx <- suppressMessages(r4sub_run_context("S1", "DEV"))
  good <- suppressMessages(as_evidence(data.frame(
    asset_type = "validation", asset_id = "DS1",
    source_name = "test", indicator_id = "IND1",
    indicator_name = "IND1", indicator_domain = "quality",
    severity = "low", result = "pass",
    stringsAsFactors = FALSE
  ), ctx = ctx))
  bad <- data.frame(x = 1)
  expect_error(suppressMessages(bind_evidence(good, bad)), "missing required column")
})

test_that("bind_evidence combines valid frames", {
  ctx <- suppressMessages(r4sub_run_context("S1", "DEV"))
  make_ev <- function(id) {
    suppressMessages(as_evidence(data.frame(
      asset_type = "validation", asset_id = "DS1",
      source_name = "test", indicator_id = id,
      indicator_name = id, indicator_domain = "quality",
      severity = "low", result = "pass",
      stringsAsFactors = FALSE
    ), ctx = ctx))
  }
  ev1 <- make_ev("IND1")
  ev2 <- make_ev("IND2")
  combined <- suppressMessages(bind_evidence(ev1, ev2))
  expect_equal(nrow(combined), 2)
})

test_that("evidence_summary handles multiple groups", {
  ctx <- suppressMessages(r4sub_run_context("S1", "DEV"))
  ev <- suppressMessages(as_evidence(data.frame(
    asset_type = rep("validation", 3),
    asset_id = rep("DS1", 3),
    source_name = c("test", "test", "other"),
    indicator_id = paste0("IND", 1:3),
    indicator_name = paste0("Test", 1:3),
    indicator_domain = c("quality", "quality", "risk"),
    severity = c("high", "low", "medium"),
    result = c("fail", "pass", "warn"),
    stringsAsFactors = FALSE
  ), ctx = ctx))
  summ <- evidence_summary(ev)
  expect_equal(nrow(summ), 3)
  expect_equal(sum(summ$n), 3L)
})

test_that("evidence_summary returns expected shape", {
  ctx <- suppressMessages(r4sub_run_context("S1", "DEV"))
  ev <- suppressMessages(as_evidence(data.frame(
    asset_type = "validation", asset_id = "DS1",
    source_name = "test", indicator_id = "IND1",
    indicator_name = "Test", indicator_domain = "quality",
    severity = "high", result = "fail",
    stringsAsFactors = FALSE
  ), ctx = ctx))
  summ <- evidence_summary(ev)
  expect_true("n" %in% names(summ))
  expect_equal(summ$n, 1L)
  expect_equal(summ$indicator_domain, "quality")
})
