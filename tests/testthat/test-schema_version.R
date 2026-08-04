# Tests for evidence schema versioning (R/schema_version.R)

ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))

make_ev <- function() {
  suppressMessages(as_evidence(
    data.frame(
      asset_type = "validation", asset_id = "ADSL",
      source_name = "pinnacle21", indicator_id = "SD0001",
      indicator_name = "SD0001", indicator_domain = "quality",
      severity = "high", result = "fail",
      stringsAsFactors = FALSE
    ),
    ctx = ctx
  ))
}

test_that("the current schema version is reported", {
  v <- evidence_schema_version()
  expect_type(v, "character")
  expect_length(v, 1L)
  expect_true(v %in% supported_evidence_schema_versions())
})

test_that("as_evidence stamps the current schema version", {
  ev <- make_ev()
  expect_equal(evidence_schema_version(ev), evidence_schema_version())
})

test_that("an unstamped table reports NA", {
  df <- data.frame(a = 1)
  expect_true(is.na(evidence_schema_version(df)))
})

test_that("a stamped current table validates without warning", {
  ev <- make_ev()
  expect_silent(validate_evidence(ev))
})

test_that("an unstamped table still validates (lenient)", {
  ev <- make_ev()
  attr(ev, "evidence_schema_version") <- NULL
  expect_silent(validate_evidence(ev))
})

test_that("a table stamped with an unknown version is rejected", {
  ev <- make_ev()
  attr(ev, "evidence_schema_version") <- "9.9.9"
  expect_error(validate_evidence(ev), "not supported")
})

test_that("bind_evidence stamps the combined table", {
  combined <- suppressMessages(bind_evidence(make_ev(), make_ev()))
  expect_equal(evidence_schema_version(combined), evidence_schema_version())
})

test_that("migrate_evidence stamps an unstamped table to current", {
  ev <- make_ev()
  attr(ev, "evidence_schema_version") <- NULL
  migrated <- migrate_evidence(ev)
  expect_equal(evidence_schema_version(migrated), evidence_schema_version())
})

test_that("migrate_evidence to the current version is a no-op on data", {
  ev <- make_ev()
  migrated <- migrate_evidence(ev)
  expect_equal(migrated$indicator_id, ev$indicator_id)
  expect_equal(nrow(migrated), nrow(ev))
})

test_that("migrate_evidence rejects an unknown target", {
  ev <- make_ev()
  expect_error(migrate_evidence(ev, to = "9.9.9"), "unknown schema version")
})

test_that("an imported RDS table keeps its version, CSV degrades to unstamped", {
  ev <- make_ev()

  rds <- tempfile(fileext = ".rds")
  suppressMessages(export_evidence(ev, rds, format = "rds"))
  ev_rds <- suppressMessages(import_evidence(rds, format = "rds"))
  expect_equal(evidence_schema_version(ev_rds), evidence_schema_version())

  csv <- tempfile(fileext = ".csv")
  suppressMessages(export_evidence(ev, csv, format = "csv"))
  ev_csv <- suppressMessages(import_evidence(csv, format = "csv"))
  expect_true(is.na(evidence_schema_version(ev_csv)))
})
