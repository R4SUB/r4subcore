# Tests for export_evidence() / import_evidence() (R/export_evidence.R)

ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))

make_ev <- function() {
  suppressMessages(as_evidence(
    data.frame(
      asset_type = "validation", asset_id = "ADSL",
      source_name = "pinnacle21", indicator_id = "SD0001",
      indicator_name = "SD0001", indicator_domain = "quality",
      severity = "high", result = "fail",
      message = "Variable AGEU is missing",
      metric_value = 0.5, metric_unit = "score",
      stringsAsFactors = FALSE
    ),
    ctx = ctx
  ))
}

test_that("CSV round-trip preserves rows and schema columns", {
  ev <- make_ev()
  csv <- tempfile(fileext = ".csv")
  suppressMessages(export_evidence(ev, csv, format = "csv"))
  ev2 <- suppressMessages(import_evidence(csv, format = "csv"))
  expect_equal(nrow(ev2), nrow(ev))
  expect_true(all(names(evidence_schema()) %in% names(ev2)))
  expect_equal(ev2$indicator_id, ev$indicator_id)
})

test_that("RDS round-trip preserves values and types", {
  ev <- make_ev()
  rds <- tempfile(fileext = ".rds")
  suppressMessages(export_evidence(ev, rds, format = "rds"))
  ev2 <- suppressMessages(import_evidence(rds, format = "rds"))
  expect_equal(ev2$metric_value, ev$metric_value)
  expect_s3_class(ev2$created_at, "POSIXct")
})

test_that("JSON round-trip reloads a valid evidence table", {
  ev <- make_ev()
  json <- tempfile(fileext = ".json")
  suppressMessages(export_evidence(ev, json, format = "json"))
  ev2 <- suppressMessages(import_evidence(json, format = "json"))
  expect_equal(nrow(ev2), nrow(ev))
  expect_equal(ev2$indicator_id, ev$indicator_id)
  expect_equal(ev2$severity, ev$severity)
  # import re-validates, so a returned table is schema-conformant
  expect_silent(validate_evidence(ev2))
})

test_that("import coerces an all-NA character column back to character", {
  # Regression guard for the NEWS 0.2.0 fix: a CSV/JSON round-trip can read an
  # all-NA character column (e.g. source_version) back as logical, which then
  # fails validation. import_evidence() must coerce schema columns to type.
  ev <- make_ev()
  expect_true(all(is.na(ev$source_version)))

  csv <- tempfile(fileext = ".csv")
  suppressMessages(export_evidence(ev, csv, format = "csv"))
  ev2 <- suppressMessages(import_evidence(csv, format = "csv"))
  expect_type(ev2$source_version, "character")
  expect_silent(validate_evidence(ev2))
})

test_that("export_evidence returns the path with traceability attributes", {
  ev <- make_ev()
  csv <- tempfile(fileext = ".csv")
  out <- suppressMessages(export_evidence(ev, csv, format = "csv"))
  expect_equal(as.character(out), csv)
  expect_s3_class(attr(out, "exported_at"), "POSIXct")
  expect_equal(attr(out, "nrow"), nrow(ev))
  expect_true(nzchar(attr(out, "r4subcore_version")))
})

test_that("import_evidence errors on a missing file", {
  expect_error(
    import_evidence(tempfile(fileext = ".csv"), format = "csv"),
    "File not found"
  )
})
