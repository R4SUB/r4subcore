# Tests for Define-XML vs dataset consistency (R/define_consistency.R)

mk_define <- function(body) {
  xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<ODM xmlns="http://www.cdisc.org/ns/odm/v1.3"\n',
    '     xmlns:def="http://www.cdisc.org/ns/def/v2.1">\n',
    '  <Study OID="S1"><MetaDataVersion OID="M1" Name="Define 2.1">\n',
    body,
    '  </MetaDataVersion></Study>\n</ODM>\n'
  )
  path <- tempfile(fileext = ".xml")
  writeLines(xml, path)
  path
}

ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))

define_body <- '
  <def:ItemGroupDef OID="IG.ADSL" Name="ADSL" def:Label="Subject Level">
    <ItemRef ItemOID="IT.ADSL.USUBJID"/>
    <ItemRef ItemOID="IT.ADSL.AGE"/>
    <ItemRef ItemOID="IT.ADSL.SEX"/>
  </def:ItemGroupDef>
  <ItemDef OID="IT.ADSL.USUBJID" Name="USUBJID" DataType="text"/>
  <ItemDef OID="IT.ADSL.AGE" Name="AGE" DataType="integer"/>
  <ItemDef OID="IT.ADSL.SEX" Name="SEX" DataType="text"/>'

test_that("define_variables extracts dataset and variable pairs", {
  dv <- define_variables(mk_define(define_body))
  expect_equal(nrow(dv), 3L)
  expect_setequal(dv$variable, c("USUBJID", "AGE", "SEX"))
  expect_true(all(dv$dataset == "ADSL"))
})

test_that("a matching data listing produces all passes", {
  data <- data.frame(dataset = "ADSL", variable = c("USUBJID", "AGE", "SEX"))
  ev <- suppressMessages(check_define_consistency(mk_define(define_body), data, ctx))
  expect_true(all(ev$result == "pass"))
  expect_silent(validate_evidence(ev))
})

test_that("a variable documented in Define but absent from the data fails", {
  data <- data.frame(dataset = "ADSL", variable = c("USUBJID", "AGE"))
  ev <- suppressMessages(check_define_consistency(mk_define(define_body), data, ctx))
  sex <- ev[ev$location == "ADSL:SEX", ]
  expect_equal(sex$result, "fail")
  expect_equal(sex$severity, "high")
  expect_match(sex$message, "absent from the data")
})

test_that("a variable present in the data but not documented fails", {
  data <- data.frame(dataset = "ADSL",
                     variable = c("USUBJID", "AGE", "SEX", "RACE"))
  ev <- suppressMessages(check_define_consistency(mk_define(define_body), data, ctx))
  race <- ev[ev$location == "ADSL:RACE", ]
  expect_equal(race$result, "fail")
  expect_match(race$message, "not documented")
})

test_that("the comparison is case-insensitive", {
  define <- data.frame(dataset = "adsl", variable = c("usubjid", "age"))
  data   <- data.frame(dataset = "ADSL", variable = c("USUBJID", "AGE"))
  ev <- suppressMessages(check_define_consistency(define, data, ctx))
  expect_true(all(ev$result == "pass"))
})

test_that("check_define_consistency accepts a define data.frame directly", {
  define <- data.frame(dataset = "ADSL", variable = c("USUBJID", "AGE", "SEX"))
  data   <- data.frame(dataset = "ADSL", variable = c("USUBJID", "AGE", "SEX"))
  ev <- suppressMessages(check_define_consistency(define, data, ctx))
  expect_equal(nrow(ev), 3L)
  expect_true(all(ev$result == "pass"))
})

test_that("missing dataset/variable columns are an error", {
  expect_error(
    check_define_consistency(
      data.frame(x = 1),
      data.frame(dataset = "A", variable = "B"),
      ctx
    ),
    "dataset"
  )
})
