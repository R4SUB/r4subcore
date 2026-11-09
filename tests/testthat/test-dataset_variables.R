# Tests for dataset_variables() (R/dataset_variables.R)

test_that("a single data frame needs and uses a dataset name", {
  out <- dataset_variables(data.frame(USUBJID = 1, AGE = 40, SEX = "F"),
                           dataset = "ADSL")
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), c("dataset", "variable"))
  expect_true(all(out$dataset == "ADSL"))
  expect_identical(out$variable, c("USUBJID", "AGE", "SEX"))
  expect_type(out$dataset, "character")
  expect_type(out$variable, "character")
})

test_that("a single data frame without a dataset name is an error", {
  expect_error(dataset_variables(data.frame(A = 1)), "dataset")
})

test_that("dataset must be a single string", {
  expect_error(
    dataset_variables(data.frame(A = 1), dataset = c("X", "Y")),
    "single"
  )
})

test_that("a named list yields one block per dataset", {
  out <- dataset_variables(list(
    ADSL = data.frame(USUBJID = 1, AGE = 40),
    ADAE = data.frame(USUBJID = 1, AEDECOD = "X", AESEV = "MILD")
  ))
  expect_equal(nrow(out), 5L)
  expect_setequal(unique(out$dataset), c("ADSL", "ADAE"))
  expect_equal(out$variable[out$dataset == "ADAE"],
               c("USUBJID", "AEDECOD", "AESEV"))
})

test_that("an unnamed or partially named list is an error", {
  expect_error(dataset_variables(list(data.frame(A = 1))), "named")
  expect_error(
    dataset_variables(list(ADSL = data.frame(A = 1), data.frame(B = 1))),
    "named"
  )
})

test_that("non-data-frame list elements are an error", {
  expect_error(
    dataset_variables(list(ADSL = data.frame(A = 1), ADAE = 1:3)),
    "data frame"
  )
})

test_that("a directory of .rds files is read, one data frame each", {
  dir <- tempfile("dsv"); dir.create(dir)
  saveRDS(data.frame(USUBJID = 1, AGE = 40), file.path(dir, "ADSL.rds"))
  saveRDS(data.frame(USUBJID = 1, AEDECOD = "X"), file.path(dir, "ADAE.rds"))
  out <- dataset_variables(dir)
  expect_equal(nrow(out), 4L)
  # File name (without extension) becomes the dataset name; order is by name.
  expect_equal(unique(out$dataset), c("ADAE", "ADSL"))
  expect_setequal(out$variable, c("USUBJID", "AGE", "AEDECOD"))
})

test_that("a directory ignores files that are not datasets", {
  dir <- tempfile("dsv"); dir.create(dir)
  saveRDS(data.frame(X = 1, Y = 2), file.path(dir, "DM.rds"))
  writeLines("not a dataset", file.path(dir, "readme.txt"))
  out <- dataset_variables(dir)
  expect_equal(out$dataset, c("DM", "DM"))
  expect_equal(out$variable, c("X", "Y"))
})

test_that("an empty directory returns an empty table", {
  dir <- tempfile("dsv"); dir.create(dir)
  out <- dataset_variables(dir)
  expect_equal(nrow(out), 0L)
  expect_identical(names(out), c("dataset", "variable"))
})

test_that("a zero-column data frame yields no rows", {
  out <- dataset_variables(data.frame()[0], dataset = "EMPTY")
  expect_equal(nrow(out), 0L)
  expect_identical(names(out), c("dataset", "variable"))
})

test_that("the result feeds check_define_consistency()", {
  ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
  define <- data.frame(dataset = "ADSL", variable = c("USUBJID", "AGE"))
  data <- dataset_variables(list(ADSL = data.frame(USUBJID = 1, AGE = 40)))
  ev <- suppressMessages(check_define_consistency(define, data, ctx))
  expect_true(all(ev$result == "pass"))
  expect_silent(validate_evidence(ev))
})

test_that(".xpt files are read when haven is available", {
  skip_if_not_installed("haven")
  dir <- tempfile("dsv"); dir.create(dir)
  haven::write_xpt(data.frame(USUBJID = 1, AGE = 40),
                   file.path(dir, "ADSL.xpt"))
  out <- dataset_variables(dir)
  expect_equal(out$dataset, c("ADSL", "ADSL"))
  expect_setequal(toupper(out$variable), c("USUBJID", "AGE"))
})

test_that("an unsupported top-level input is an error", {
  expect_error(dataset_variables(1:3), "data frame")
})
