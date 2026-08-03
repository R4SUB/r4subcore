# Tests for the ADRG evidence source (R/parsers_adrg.R)

test_that("adrg_sections lists the required top-level sections", {
  s <- adrg_sections()
  expect_s3_class(s, "tbl_df")
  expect_true(all(c("section_id", "section_name", "required") %in% names(s)))
  expect_equal(sum(s$required), 6L)
})

test_that("a complete guide passes every section", {
  ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
  all_ids <- adrg_sections()$section_id
  ev <- suppressMessages(adrg_to_evidence(all_ids, ctx))

  expect_equal(nrow(ev), length(all_ids))
  expect_true(all(ev$result == "pass"))
  expect_true(all(ev$indicator_domain == "usability"))
  expect_true(all(ev$source_name == "adrg"))
  expect_silent(validate_evidence(ev))
})

test_that("a missing required section fails at high severity", {
  ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
  # Provide everything except section 6 (required).
  ids <- setdiff(adrg_sections()$section_id, "6")
  ev <- suppressMessages(adrg_to_evidence(ids, ctx))

  sec6 <- ev[ev$location == "ADRG:6", ]
  expect_equal(sec6$result, "fail")
  expect_equal(sec6$severity, "high")
})

test_that("a missing recommended section warns at low severity", {
  ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
  # Only the six required sections present; recommended ones missing.
  ids <- adrg_sections()$section_id[adrg_sections()$required]
  ev <- suppressMessages(adrg_to_evidence(ids, ctx))

  sec34 <- ev[ev$location == "ADRG:3.4", ]
  expect_equal(sec34$result, "warn")
  expect_equal(sec34$severity, "low")
})

test_that("a data.frame of section presence is accepted", {
  ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
  df <- data.frame(
    section_id = c("1", "2", "3"),
    present    = c(TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  ev <- suppressMessages(adrg_to_evidence(df, ctx))

  expect_equal(ev$result[ev$location == "ADRG:1"], "pass")
  expect_equal(ev$result[ev$location == "ADRG:3"], "fail")
})

test_that("unknown section ids warn but do not error", {
  ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
  expect_warning(
    suppressMessages(adrg_to_evidence(c("1", "99"), ctx)),
    "unknown"
  )
})

test_that("sdrg can reuse the same machinery via source_name", {
  ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
  ev <- suppressMessages(adrg_to_evidence(c("1", "2"), ctx, source_name = "sdrg"))
  expect_true(all(ev$source_name == "sdrg"))
})

test_that("evidence_sources documents ADRG as supported", {
  src <- evidence_sources()
  expect_s3_class(src, "tbl_df")
  adrg_row <- src[grepl("ADRG", src$source), ]
  expect_equal(adrg_row$status, "supported")
  expect_equal(adrg_row$pillar, "usability")
})
