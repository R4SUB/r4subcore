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

# --- Additional coverage ----------------------------------------------------

# A hand-built table that trips (almost) every check at once. Confirmed to
# fire: missing_required_column, wrong_type, na_in_required, invalid_value
# (twice), invalid_payload_json, missing_schema_version, duplicate_key,
# metric_value_without_unit, metric_unit_without_value.
mega <- function() {
  ev <- good()[rep(1, 2), ]
  attr(ev, "evidence_schema_version") <- NULL     # missing_schema_version note
  ev$study_id <- NULL                              # missing_required_column
  ev$created_at <- as.character(ev$created_at)     # wrong_type error
  ev$source_name <- c(NA, "pinnacle21")            # na_in_required error
  ev$severity <- c("oops", "high")                 # invalid_value error
  ev$result <- c("nope", "pass")                   # invalid_value error
  ev$evidence_payload <- c("bad {", "{}")          # invalid_payload_json warning
  ev$metric_value <- c(0.5, NA_real_)              # row 1 value, no unit
  ev$metric_unit  <- c(NA_character_, "pct")       # row 2 unit, no value
  ev                                               # duplicate (SD0001, ADSL) key
}

test_that("a zero-row but schema-complete table lints clean", {
  ev <- good()[0, , drop = FALSE]
  attr(ev, "evidence_schema_version") <- "1.0.0"
  out <- lint_evidence(ev)
  expect_equal(nrow(out), 0L)
  expect_identical(names(out), c("code", "level", "field", "n", "message"))
  expect_equal(unname(attr(out, "summary")[c("error", "warning", "note")]),
               c(0L, 0L, 0L))
})

test_that("an empty checks vector runs nothing, even on a broken table", {
  out <- lint_evidence(mega(), checks = character(0))
  expect_equal(nrow(out), 0L)
  expect_identical(names(out), c("code", "level", "field", "n", "message"))
})

test_that("every simultaneous issue is reported with exact level/field/n", {
  out <- lint_evidence(mega())
  expect_equal(nrow(out), 10L)

  # exact per-finding values
  expect_equal(out$field[out$code == "missing_required_column"], "study_id")
  expect_true(is.na(out$n[out$code == "missing_required_column"]))
  expect_equal(out$field[out$code == "wrong_type"], "created_at")
  expect_equal(out$field[out$code == "na_in_required"], "source_name")
  expect_equal(out$n[out$code == "na_in_required"], 1L)

  iv <- out[out$code == "invalid_value", ]
  expect_setequal(iv$field, c("severity", "result"))
  expect_true(all(iv$n == 1L))
  expect_true(all(iv$level == "error"))

  expect_equal(out$level[out$code == "invalid_payload_json"], "warning")
  expect_equal(out$n[out$code == "invalid_payload_json"], 1L)

  # the summary attribute counts each level exactly
  s <- attr(out, "summary")
  expect_type(s, "integer")
  expect_identical(names(s), c("error", "warning", "note"))
  expect_equal(unname(s), c(5L, 1L, 4L))
  expect_equal(s[["error"]], sum(out$level == "error"))
})

test_that("findings are ordered error, then warning, then note", {
  out <- lint_evidence(mega())
  rank <- match(out$level, c("error", "warning", "note"))
  expect_false(is.unsorted(rank))
  expect_equal(out$level[1], "error")
  expect_equal(out$level[nrow(out)], "note")
})

test_that("each checks= subset returns only that check's findings", {
  ev <- mega()
  full <- lint_evidence(ev)
  for (code in unique(full$code)) {
    sub <- lint_evidence(ev, checks = code)
    expect_true(all(sub$code == code))
    expect_equal(nrow(sub), sum(full$code == code))
  }
  # a multi-code subset returns exactly the firing subset of those codes
  two <- lint_evidence(ev, checks = c("na_in_required", "invalid_value"))
  expect_setequal(unique(two$code), c("na_in_required", "invalid_value"))
  expect_equal(nrow(two),
               sum(full$code %in% c("na_in_required", "invalid_value")))
})

test_that("table-level findings carry NA field and NA n", {
  ev <- good()
  attr(ev, "evidence_schema_version") <- NULL
  row <- lint_evidence(ev)[lint_evidence(ev)$code == "missing_schema_version", ]
  expect_true(is.na(row$field))
  expect_true(is.na(row$n))
})

test_that("an unknown schema version reports NA field/n and names the version", {
  ev <- good()
  attr(ev, "evidence_schema_version") <- "99.0.0"
  row <- lint_evidence(ev)[lint_evidence(ev)$code == "unknown_schema_version", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$level, "error")
  expect_true(is.na(row$field))
  expect_true(is.na(row$n))
  expect_match(row$message, "99.0.0")
})

test_that("duplicate_key counts rows across several distinct keys", {
  ev <- good()[rep(1, 4), ]
  attr(ev, "evidence_schema_version") <- "1.0.0"
  ev$indicator_id <- c("A", "A", "B", "B")
  ev$asset_id <- "X"
  row <- lint_evidence(ev)[lint_evidence(ev)$code == "duplicate_key", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$level, "note")
  expect_equal(row$n, 4L)         # all four rows sit in a duplicated key
  expect_true(is.na(row$field))
  expect_match(row$message, "2 \\(indicator_id, asset_id\\) key")
})

test_that("unique keys produce no duplicate_key note", {
  ev <- good()[rep(1, 3), ]
  attr(ev, "evidence_schema_version") <- "1.0.0"
  ev$indicator_id <- c("A", "B", "C")
  out <- lint_evidence(ev)
  expect_equal(nrow(out[out$code == "duplicate_key", ]), 0L)
})

test_that("invalid_payload_json counts only malformed cells, skipping NA/empty", {
  ev <- good()[rep(1, 4), ]
  attr(ev, "evidence_schema_version") <- "1.0.0"
  ev$indicator_id <- paste0("I", 1:4)
  # valid JSON, malformed, empty string, NA
  ev$evidence_payload <- c("{}", "nope {", "", NA_character_)
  row <- lint_evidence(ev)[lint_evidence(ev)$code == "invalid_payload_json", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$n, 1L)
  expect_match(row$message, "^1 evidence_payload cell is")
})

test_that("invalid_payload_json pluralizes its message for several bad cells", {
  ev <- good()[rep(1, 2), ]
  attr(ev, "evidence_schema_version") <- "1.0.0"
  ev$indicator_id <- c("I1", "I2")
  ev$evidence_payload <- c("bad {", "also bad [")
  row <- lint_evidence(ev)[lint_evidence(ev)$code == "invalid_payload_json", ]
  expect_equal(row$n, 2L)
  expect_match(row$message, "^2 evidence_payload cells are")
})

test_that("a non-character evidence_payload is wrong_type, not invalid JSON", {
  ev <- good()
  ev$evidence_payload <- 42
  out <- lint_evidence(ev)
  expect_equal(nrow(out[out$code == "invalid_payload_json", ]), 0L)
  expect_equal(nrow(out[out$code == "wrong_type" &
                          out$field == "evidence_payload", ]), 1L)
})

test_that("an NA in a vocabulary column is na_in_required, not invalid_value", {
  ev <- good()[rep(1, 2), ]
  attr(ev, "evidence_schema_version") <- "1.0.0"
  ev$indicator_id <- c("I1", "I2")
  ev$severity <- c("high", NA_character_)
  out <- lint_evidence(ev)
  sev <- out[out$field == "severity", ]
  expect_equal(nrow(sev), 1L)
  expect_equal(sev$code, "na_in_required")
  expect_equal(sev$n, 1L)
  expect_equal(nrow(out[out$code == "invalid_value" & out$field == "severity", ]),
               0L)
})

test_that("na_in_required counts every NA and fires per column", {
  ev <- good()[rep(1, 3), ]
  attr(ev, "evidence_schema_version") <- "1.0.0"
  ev$indicator_id <- c("I1", "I2", "I3")
  ev$asset_id <- c(NA, NA, "ADSL")      # two NAs in one required column
  ev$source_name <- c(NA, "p", "p")     # one NA in another required column
  out <- lint_evidence(ev)
  a <- out[out$code == "na_in_required" & out$field == "asset_id", ]
  s <- out[out$code == "na_in_required" & out$field == "source_name", ]
  expect_equal(a$n, 2L)
  expect_equal(s$n, 1L)
  expect_match(a$message, "2 NA values")
})

test_that("wrong_type fires for both a POSIXct and a double column", {
  ev <- good()
  ev$created_at <- as.character(ev$created_at)  # should be POSIXct
  ev$metric_value <- "0.5"                       # should be double
  out <- lint_evidence(ev)
  wt <- out[out$code == "wrong_type", ]
  expect_setequal(wt$field, c("created_at", "metric_value"))
  expect_true(all(wt$level == "error"))
})

test_that("both metric completeness notes fire together on different rows", {
  ev <- good()[rep(1, 2), ]
  attr(ev, "evidence_schema_version") <- "1.0.0"
  ev$indicator_id <- c("I1", "I2")
  ev$metric_value <- c(0.5, NA_real_)
  ev$metric_unit  <- c(NA_character_, "pct")
  out <- lint_evidence(ev)
  vnu <- out[out$code == "metric_value_without_unit", ]
  unv <- out[out$code == "metric_unit_without_value", ]
  expect_equal(vnu$n, 1L)
  expect_equal(vnu$field, "metric_unit")
  expect_equal(vnu$level, "note")
  expect_equal(unv$n, 1L)
  expect_equal(unv$field, "metric_value")
  expect_equal(unv$level, "note")
})

test_that("checks rejects several unknown codes and non-character input", {
  ev <- good()
  expect_error(lint_evidence(ev, checks = c("nope", "nah")), "Unknown check")
  expect_error(lint_evidence(ev, checks = 1:3), "character")
})
