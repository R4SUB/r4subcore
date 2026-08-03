# Tests for evidence_schema_spec (R/evidence_schema_spec.R)

test_that("spec has one row per schema column in order", {
  spec <- evidence_schema_spec()
  schema <- evidence_schema()

  expect_s3_class(spec, "tbl_df")
  expect_equal(spec$column, names(schema))
  expect_true(all(c("column", "type", "required", "allowed_values",
                    "description") %in% names(spec)))
})

test_that("required flags match the schema", {
  spec <- evidence_schema_spec()

  expect_true(spec$required[spec$column == "run_id"])
  expect_true(spec$required[spec$column == "result"])
  expect_false(spec$required[spec$column == "source_version"])
  expect_false(spec$required[spec$column == "message"])
})

test_that("controlled vocabularies are listed, free text is NA", {
  spec <- evidence_schema_spec()

  sev <- spec$allowed_values[spec$column == "severity"]
  expect_match(sev, "critical")
  res <- spec$allowed_values[spec$column == "result"]
  expect_match(res, "pass")

  expect_true(is.na(spec$allowed_values[spec$column == "message"]))
  expect_true(is.na(spec$allowed_values[spec$column == "asset_id"]))
})

test_that("every column has a non-empty description", {
  spec <- evidence_schema_spec()
  expect_false(any(is.na(spec$description)))
  expect_true(all(nchar(spec$description) > 0))
})

test_that("allowed values agree with the schema", {
  spec <- evidence_schema_spec()
  schema <- evidence_schema()

  for (col in names(schema)) {
    allowed <- schema[[col]]$allowed
    cell <- spec$allowed_values[spec$column == col]
    if (is.null(allowed)) {
      expect_true(is.na(cell))
    } else {
      expect_equal(strsplit(cell, ", ")[[1]], allowed)
    }
  }
})
