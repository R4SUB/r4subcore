# Tests for lint_evidence (R/lint_evidence.R)

ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))

good <- function() {
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

test_that("a valid evidence table lints clean", {
  out <- lint_evidence(good())
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), c("code", "level", "field", "n", "message"))
  expect_equal(nrow(out), 0L)
  expect_type(out$n, "integer")
  s <- attr(out, "summary")
  expect_equal(unname(s[c("error", "warning", "note")]), c(0L, 0L, 0L))
})

test_that("non data.frame input errors", {
  expect_error(lint_evidence(42), "data.frame")
})

test_that("missing required column is an error finding", {
  ev <- good()
  ev$study_id <- NULL
  out <- lint_evidence(ev)
  row <- out[out$code == "missing_required_column", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$level, "error")
  expect_equal(row$field, "study_id")
})

test_that("NA in a required column is counted", {
  ev <- good()
  ev <- rbind(ev, ev)
  ev$result[1] <- NA
  out <- lint_evidence(ev)
  row <- out[out$code == "na_in_required" & out$field == "result", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$level, "error")
  expect_equal(row$n, 1L)
})

test_that("values outside the vocabulary are reported with a count", {
  ev <- good()
  ev <- rbind(ev, ev)
  ev$severity <- c("sky-high", "sky-high")
  out <- lint_evidence(ev)
  row <- out[out$code == "invalid_value" & out$field == "severity", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$level, "error")
  expect_equal(row$n, 2L)
  expect_match(row$message, "sky-high")
})

test_that("a wrong column type is an error", {
  ev <- good()
  ev$metric_value <- as.character(ev$metric_value)
  out <- lint_evidence(ev)
  row <- out[out$code == "wrong_type" & out$field == "metric_value", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$level, "error")
})

test_that("lint never throws on a badly malformed table", {
  bad <- data.frame(foo = 1:3, bar = letters[1:3], stringsAsFactors = FALSE)
  expect_no_error(out <- lint_evidence(bad))
  # several required columns absent
  expect_true(sum(out$code == "missing_required_column") > 1)
  expect_true(all(out$level[out$code == "missing_required_column"] == "error"))
})

test_that("missing schema-version stamp is a note", {
  ev <- good()
  attr(ev, "evidence_schema_version") <- NULL
  out <- lint_evidence(ev)
  row <- out[out$code == "missing_schema_version", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$level, "note")
})

test_that("an unknown schema version is an error", {
  ev <- good()
  attr(ev, "evidence_schema_version") <- "99.0.0"
  out <- lint_evidence(ev)
  row <- out[out$code == "unknown_schema_version", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$level, "error")
})

test_that("invalid evidence_payload JSON is a warning", {
  ev <- good()
  ev$evidence_payload <- "not json {"
  out <- lint_evidence(ev)
  row <- out[out$code == "invalid_payload_json", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$level, "warning")
  expect_equal(row$n, 1L)

  # valid JSON (the default "{}") does not fire
  ev2 <- good()
  expect_equal(nrow(lint_evidence(ev2)[lint_evidence(ev2)$code == "invalid_payload_json", ]), 0L)
})

test_that("duplicate (indicator_id, asset_id) keys are a note", {
  ev <- suppressMessages(bind_evidence(good(), good()))
  out <- lint_evidence(ev)
  row <- out[out$code == "duplicate_key", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$level, "note")
  expect_equal(row$n, 2L)
})

test_that("metric value/unit completeness notes fire independently", {
  ev <- good()
  ev$metric_value <- 0.5
  ev$metric_unit <- NA_character_
  out <- lint_evidence(ev)
  expect_equal(nrow(out[out$code == "metric_value_without_unit", ]), 1L)
  expect_equal(nrow(out[out$code == "metric_unit_without_value", ]), 0L)

  ev2 <- good()
  ev2$metric_value <- NA_real_
  ev2$metric_unit <- "pct"
  out2 <- lint_evidence(ev2)
  expect_equal(nrow(out2[out2$code == "metric_unit_without_value", ]), 1L)
  expect_equal(nrow(out2[out2$code == "metric_value_without_unit", ]), 0L)
})

test_that("findings are ordered most serious first and summarised", {
  ev <- good()
  ev$severity <- "oops"              # error
  ev$metric_value <- 1               # note (no unit)
  attr(ev, "evidence_schema_version") <- NULL  # note
  out <- lint_evidence(ev)
  expect_equal(out$level[1], "error")
  expect_true(all(diff(match(out$level, c("error", "warning", "note"))) >= 0))
  s <- attr(out, "summary")
  expect_equal(s[["error"]], sum(out$level == "error"))
  expect_equal(s[["note"]], sum(out$level == "note"))
})

test_that("checks argument selects a subset and rejects unknown codes", {
  ev <- good()
  ev$severity <- "oops"
  attr(ev, "evidence_schema_version") <- NULL
  out <- lint_evidence(ev, checks = "invalid_value")
  expect_true(all(out$code == "invalid_value"))
  expect_equal(nrow(out[out$code == "missing_schema_version", ]), 0L)

  expect_error(lint_evidence(ev, checks = "nope"), "Unknown check")
})
