test_that("validate_indicator passes for valid input", {
  expect_true(validate_indicator(list(
    indicator_id = "IND-001",
    domain = "quality",
    description = "Test indicator"
  )))
})

test_that("validate_indicator catches missing fields", {
  expect_error(
    validate_indicator(list(indicator_id = "IND-001")),
    "missing required field"
  )
})

test_that("validate_indicator catches invalid domain", {
  expect_error(
    validate_indicator(list(
      indicator_id = "IND-001",
      domain = "bogus",
      description = "test"
    )),
    "domain must be"
  )
})

test_that("register_indicator adds to registry", {
  suppressMessages(
    register_indicator("TEST-IND-001", "quality", "Test indicator")
  )
  # Internal: verify it's stored
  ind <- r4subcore:::get_indicator("TEST-IND-001")
  expect_equal(ind$indicator_id, "TEST-IND-001")
  expect_equal(ind$domain, "quality")
})

test_that("register_indicator validates before registering", {
  expect_error(
    register_indicator("BAD", "invalid_domain", "desc"),
    "domain must be"
  )
})
