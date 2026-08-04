# Tests for read_p21_report (R/read_p21_report.R)

fixture <- function(name) test_path("fixtures", name)

test_that("a CSV report is read with its original column names", {
  df <- suppressMessages(read_p21_report(fixture("p21_sample.csv")))
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3L)
  expect_true("Rule ID" %in% names(df))
  expect_equal(df[["Rule ID"]], c("SD0001", "SD0002", "AD0018"))
})

test_that("a CSV report flows into evidence via p21_to_evidence", {
  ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
  df <- suppressMessages(read_p21_report(fixture("p21_sample.csv")))
  ev <- suppressMessages(p21_to_evidence(df, ctx))
  expect_silent(validate_evidence(ev))
  expect_equal(ev$indicator_id, c("SD0001", "SD0002", "AD0018"))
  expect_equal(ev$result, c("fail", "warn", "pass"))
  # P21 "Notice" maps onto the info severity.
  expect_equal(ev$severity, c("high", "medium", "info"))
})

test_that("format is inferred from the extension", {
  df <- suppressMessages(
    read_p21_report(fixture("p21_sample.csv"), format = "auto")
  )
  expect_equal(nrow(df), 3L)
})

test_that("a missing file is an error", {
  expect_error(read_p21_report(tempfile(fileext = ".csv")), "not found")
})

test_that("an unknown extension is an error", {
  bad <- tempfile(fileext = ".dat")
  writeLines("x", bad)
  expect_error(read_p21_report(bad), "infer the format")
})

test_that("an Excel report is read from the findings sheet", {
  skip_if_not_installed("readxl")
  # The fixture has a summary sheet first and a "Details" sheet; the reader
  # should pick the findings sheet, not the first one.
  df <- suppressMessages(read_p21_report(fixture("p21_sample.xlsx")))
  expect_equal(nrow(df), 3L)
  expect_true("Rule ID" %in% names(df))
  expect_equal(df[["Rule ID"]], c("SD0001", "SD0002", "AD0018"))
})
