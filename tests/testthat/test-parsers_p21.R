test_that("p21_to_evidence maps P21 fields correctly", {
  ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))

  p21_raw <- data.frame(
    Rule = c("SD0001", "SD0002"),
    Message = c("Missing variable label", "Invalid format"),
    Severity = c("Error", "Warning"),
    Dataset = c("ADSL", "ADAE"),
    Variable = c("AGE", "AESTDTC"),
    Status = c("Failed", "Warning"),
    stringsAsFactors = FALSE
  )

  ev <- suppressMessages(p21_to_evidence(p21_raw, ctx))
  expect_equal(nrow(ev), 2)
  expect_equal(ev$indicator_id, c("SD0001", "SD0002"))
  expect_equal(ev$severity, c("high", "medium"))
  expect_equal(ev$result, c("fail", "warn"))
  expect_equal(ev$location, c("ADSL:AGE", "ADAE:AESTDTC"))
  expect_equal(ev$source_name, c("pinnacle21", "pinnacle21"))
  expect_equal(ev$study_id, c("STUDY1", "STUDY1"))
})

test_that("p21_to_evidence handles missing columns gracefully", {
  ctx <- suppressMessages(r4sub_run_context("S1", "DEV"))

  p21_minimal <- data.frame(
    Rule = c("R1"),
    Message = c("Some issue"),
    Severity = c("Note"),
    stringsAsFactors = FALSE
  )

  ev <- suppressMessages(p21_to_evidence(p21_minimal, ctx))
  expect_equal(nrow(ev), 1)
  expect_equal(ev$indicator_id, "R1")
  expect_equal(ev$severity, "info")
  # No dataset/variable => location should be NA
  expect_true(is.na(ev$location))
  # No status => default "fail"
  expect_equal(ev$result, "fail")
})

test_that("p21_to_evidence uses source_version when provided", {
  ctx <- suppressMessages(r4sub_run_context("S1", "DEV"))
  p21_raw <- data.frame(
    Rule = "R1",
    Message = "test",
    Severity = "Low",
    Dataset = "DM",
    Status = "Passed",
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(p21_to_evidence(p21_raw, ctx, source_version = "P21-3.1.2"))
  expect_equal(ev$source_version, "P21-3.1.2")
})

test_that("p21_to_evidence with no rule column generates hash IDs", {
  ctx <- suppressMessages(r4sub_run_context("S1", "DEV"))
  p21_raw <- data.frame(
    Message = c("issue A", "issue B"),
    Severity = c("Low", "High"),
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(p21_to_evidence(p21_raw, ctx))
  expect_equal(nrow(ev), 2)
  expect_true(all(grepl("^IND-", ev$indicator_id)))
})

test_that("p21_to_evidence stores evidence_payload as JSON", {
  ctx <- suppressMessages(r4sub_run_context("S1", "DEV"))

  p21_raw <- data.frame(
    Rule = "R1",
    Message = "test",
    Severity = "Low",
    Dataset = "DM",
    Status = "Passed",
    stringsAsFactors = FALSE
  )

  ev <- suppressMessages(p21_to_evidence(p21_raw, ctx))
  payload <- jsonlite::fromJSON(ev$evidence_payload)
  expect_true("Rule" %in% names(payload))
  expect_equal(payload$Rule, "R1")
})
