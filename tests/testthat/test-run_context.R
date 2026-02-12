test_that("r4sub_run_context generates a run_id when NULL", {
  ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
  expect_true(is.character(ctx$run_id))
  expect_true(nzchar(ctx$run_id))
  expect_match(ctx$run_id, "^R4S-")
})

test_that("r4sub_run_context uses provided run_id", {
  ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV", run_id = "MY-RUN-001"))
  expect_equal(ctx$run_id, "MY-RUN-001")
})

test_that("r4sub_run_context validates environment", {
  expect_error(r4sub_run_context("S1", environment = "INVALID"))
})

test_that("r4sub_run_context stores all fields", {
  ctx <- suppressMessages(
    r4sub_run_context("ABC", "PROD", user = "testuser")
  )
  expect_equal(ctx$study_id, "ABC")
  expect_equal(ctx$environment, "PROD")
  expect_equal(ctx$user, "testuser")
  expect_s3_class(ctx, "r4sub_run_context")
  expect_true(inherits(ctx$created_at, "POSIXct"))
})

test_that("r4sub_run_context generates unique run_ids", {
  ctx1 <- suppressMessages(r4sub_run_context("S1", "DEV"))
  ctx2 <- suppressMessages(r4sub_run_context("S1", "DEV"))
  expect_false(ctx1$run_id == ctx2$run_id)
})
