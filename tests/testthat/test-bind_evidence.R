ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))

# Build a one-row-per-element evidence table from parallel vectors.
mk <- function(indicator_id, result = "pass", asset_id = "ADSL",
               location = NA_character_, severity = "low",
               source_name = "p21") {
  n <- length(indicator_id)
  suppressMessages(as_evidence(
    data.frame(
      asset_type       = rep("validation", n),
      asset_id         = rep(asset_id, length.out = n),
      source_name      = rep(source_name, length.out = n),
      indicator_id     = indicator_id,
      indicator_name   = indicator_id,
      indicator_domain = rep("quality", n),
      severity         = rep(severity, length.out = n),
      result           = rep(result, length.out = n),
      location         = rep(location, length.out = n),
      stringsAsFactors = FALSE
    ),
    ctx = ctx
  ))
}

# ---- evidence_collisions --------------------------------------------------

test_that("evidence_collisions returns a typed zero-row frame when none collide", {
  ev <- mk(c("I1", "I2", "I3"), location = c("A", "B", "C"))
  col <- evidence_collisions(ev)
  expect_s3_class(col, "data.frame")
  expect_equal(nrow(col), 0L)
  expect_equal(
    names(col),
    c("indicator_id", "asset_id", "location", "n", "n_distinct",
      "conflict", "values")
  )
  expect_type(col$conflict, "logical")
})

test_that("evidence_collisions flags a duplicate with matching result (no conflict)", {
  ev <- mk(c("I1", "I1", "I2"), result = c("fail", "fail", "pass"),
           location = c("AGE", "AGE", "SEX"))
  col <- evidence_collisions(ev)
  expect_equal(nrow(col), 1L)
  expect_equal(col$indicator_id, "I1")
  expect_equal(col$n, 2L)
  expect_equal(col$n_distinct, 1L)
  expect_false(col$conflict)
  expect_equal(col$values, "fail")
})

test_that("evidence_collisions flags a conflict when results differ", {
  ev <- mk(c("I1", "I1"), result = c("fail", "warn"),
           location = c("AGE", "AGE"))
  col <- evidence_collisions(ev)
  expect_equal(nrow(col), 1L)
  expect_true(col$conflict)
  expect_equal(col$n_distinct, 2L)
  expect_equal(col$values, "fail, warn")  # sorted
})

test_that("evidence_collisions groups NA location together", {
  # Same indicator + asset, both NA location -> collide.
  ev <- mk(c("I1", "I1"), result = c("pass", "fail"))
  col <- evidence_collisions(ev)
  expect_equal(nrow(col), 1L)
  expect_true(is.na(col$location))
  expect_true(col$conflict)
})

test_that("evidence_collisions honours a custom key and conflict column", {
  ev <- mk(c("I1", "I1"), result = c("pass", "pass"), severity = "low")
  ev$severity <- c("low", "high")
  col <- evidence_collisions(ev, keys = c("indicator_id", "asset_id"),
                             conflict_on = "severity")
  expect_equal(nrow(col), 1L)
  expect_true(col$conflict)
  expect_equal(col$values, "high, low")
})

test_that("evidence_collisions errors on an unknown column", {
  ev <- mk("I1")
  expect_error(evidence_collisions(ev, keys = "nope"), "not found")
  expect_error(evidence_collisions(ev, conflict_on = "nope"), "not found")
})

# ---- bind_evidence --------------------------------------------------------

test_that("bind_evidence keeps all rows by default and attaches a report", {
  a <- mk("I1")
  b <- mk("I2")
  out <- suppressMessages(bind_evidence(a, b))
  expect_equal(nrow(out), 2L)
  expect_s3_class(attr(out, "evidence_collisions"), "data.frame")
  expect_equal(nrow(attr(out, "evidence_collisions")), 0L)
  expect_silent(validate_evidence(out))
  expect_equal(evidence_schema_version(out), current_evidence_schema_version())
})

test_that("bind_evidence reports collisions but keeps rows under 'keep'", {
  a <- mk("I1", result = "fail", location = "AGE")
  b <- mk("I1", result = "warn", location = "AGE")
  out <- suppressMessages(bind_evidence(a, b))
  expect_equal(nrow(out), 2L)
  rep <- attr(out, "evidence_collisions")
  expect_equal(nrow(rep), 1L)
  expect_true(rep$conflict)
})

test_that("bind_evidence on_duplicate = 'first' keeps the first colliding row", {
  a <- mk("I1", result = "fail", location = "AGE")
  b <- mk("I1", result = "warn", location = "AGE")
  out <- suppressMessages(bind_evidence(a, b, on_duplicate = "first"))
  expect_equal(nrow(out), 1L)
  expect_equal(out$result, "fail")
  expect_silent(validate_evidence(out))
})

test_that("bind_evidence on_duplicate = 'last' keeps the last colliding row", {
  a <- mk("I1", result = "fail", location = "AGE")
  b <- mk("I1", result = "warn", location = "AGE")
  out <- suppressMessages(bind_evidence(a, b, on_duplicate = "last"))
  expect_equal(nrow(out), 1L)
  expect_equal(out$result, "warn")
})

test_that("bind_evidence on_duplicate = 'error' aborts on collision", {
  a <- mk("I1", location = "AGE")
  b <- mk("I1", location = "AGE")
  expect_error(
    suppressMessages(bind_evidence(a, b, on_duplicate = "error")),
    "collide"
  )
})

test_that("bind_evidence accepts a single list of frames", {
  out <- suppressMessages(bind_evidence(list(mk("I1"), mk("I2"))))
  expect_equal(nrow(out), 2L)
})

test_that("bind_evidence errors with no inputs and rejects invalid frames", {
  expect_error(suppressMessages(bind_evidence()), "at least one")
  expect_error(
    suppressMessages(bind_evidence(mk("I1"), data.frame(x = 1))),
    "missing required column"
  )
})

test_that("bind_evidence does not dedup rows that differ on location", {
  a <- mk("I1", location = "AGE")
  b <- mk("I1", location = "SEX")
  out <- suppressMessages(bind_evidence(a, b, on_duplicate = "first"))
  expect_equal(nrow(out), 2L)  # distinct keys -> not a collision
})
