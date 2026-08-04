# Tests for the Define-XML parser hardening (R/parsers_define.R)

write_define <- function(body) {
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

q3_rows <- function(ev) ev[ev$indicator_id == "Q-DEFINE-003", ]

test_that("a MethodDef derivation satisfies Q-DEFINE-003", {
  # Origin says Derived but carries no inline text; the derivation lives in a
  # referenced MethodDef. Before hardening this was a false failure.
  path <- write_define(
    '  <def:ItemGroupDef OID="IG.ADSL" Name="ADSL" def:Label="Subject Level">
       <ItemRef ItemOID="IT.ADSL.TRTSDT" Mandatory="Yes" MethodOID="MT.TRTSDT"/>
     </def:ItemGroupDef>
     <ItemDef OID="IT.ADSL.TRTSDT" Name="TRTSDT" DataType="date">
       <Description><TranslatedText>Treatment Start Date</TranslatedText></Description>
       <def:Origin Type="Derived"/>
     </ItemDef>
     <def:MethodDef OID="MT.TRTSDT" Name="M" Type="Computation">
       <Description><TranslatedText>First exposure date from EX</TranslatedText></Description>
     </def:MethodDef>')

  ev <- suppressMessages(define_xml_to_evidence(path, ctx))
  row <- q3_rows(ev)
  expect_equal(nrow(row), 1L)
  expect_equal(row$result, "pass")
  expect_match(row$message, "First exposure date")
})

test_that("an inline Origin derivation still passes", {
  path <- write_define(
    '  <def:ItemGroupDef OID="IG.ADSL" Name="ADSL" def:Label="Subject Level">
       <ItemRef ItemOID="IT.ADSL.AGE" Mandatory="Yes"/>
     </def:ItemGroupDef>
     <ItemDef OID="IT.ADSL.AGE" Name="AGE" DataType="integer">
       <Description><TranslatedText>Age</TranslatedText></Description>
       <def:Origin Type="Derived">
         <def:Description><TranslatedText>Derived from RFSTDTC</TranslatedText></def:Description>
       </def:Origin>
     </ItemDef>')

  ev <- suppressMessages(define_xml_to_evidence(path, ctx))
  expect_equal(q3_rows(ev)$result, "pass")
})

test_that("a derived variable with no derivation anywhere fails", {
  path <- write_define(
    '  <def:ItemGroupDef OID="IG.ADSL" Name="ADSL" def:Label="Subject Level">
       <ItemRef ItemOID="IT.ADSL.X" Mandatory="Yes"/>
     </def:ItemGroupDef>
     <ItemDef OID="IT.ADSL.X" Name="X" DataType="text">
       <Description><TranslatedText>X</TranslatedText></Description>
       <def:Origin Type="Derived"/>
     </ItemDef>')

  ev <- suppressMessages(define_xml_to_evidence(path, ctx))
  row <- q3_rows(ev)
  expect_equal(row$result, "fail")
  expect_equal(row$severity, "high")
})

test_that("a non-derived variable produces no derivation row", {
  path <- write_define(
    '  <def:ItemGroupDef OID="IG.ADSL" Name="ADSL" def:Label="Subject Level">
       <ItemRef ItemOID="IT.ADSL.USUBJID" Mandatory="Yes"/>
     </def:ItemGroupDef>
     <ItemDef OID="IT.ADSL.USUBJID" Name="USUBJID" DataType="text">
       <Description><TranslatedText>Unique Subject Identifier</TranslatedText></Description>
     </ItemDef>')

  ev <- suppressMessages(define_xml_to_evidence(path, ctx))
  expect_equal(nrow(q3_rows(ev)), 0L)
})

test_that("a dataset without a label fails Q-DEFINE-001", {
  path <- write_define(
    '  <def:ItemGroupDef OID="IG.ADSL" Name="ADSL">
       <ItemRef ItemOID="IT.ADSL.AGE" Mandatory="Yes"/>
     </def:ItemGroupDef>
     <ItemDef OID="IT.ADSL.AGE" Name="AGE" DataType="integer">
       <Description><TranslatedText>Age</TranslatedText></Description>
     </ItemDef>')

  ev <- suppressMessages(define_xml_to_evidence(path, ctx))
  d1 <- ev[ev$indicator_id == "Q-DEFINE-001", ]
  expect_equal(d1$result, "fail")
})

test_that("a file with no datasets emits one critical failure", {
  path <- write_define("")
  ev <- suppressMessages(define_xml_to_evidence(path, ctx))
  d1 <- ev[ev$indicator_id == "Q-DEFINE-001", ]
  expect_equal(nrow(d1), 1L)
  expect_equal(d1$severity, "critical")
  expect_equal(d1$result, "fail")
})

test_that("the parser output is valid evidence", {
  path <- write_define(
    '  <def:ItemGroupDef OID="IG.ADSL" Name="ADSL" def:Label="Subject Level">
       <ItemRef ItemOID="IT.ADSL.AGE" Mandatory="Yes"/>
     </def:ItemGroupDef>
     <ItemDef OID="IT.ADSL.AGE" Name="AGE" DataType="integer">
       <Description><TranslatedText>Age</TranslatedText></Description>
     </ItemDef>')

  ev <- suppressMessages(define_xml_to_evidence(path, ctx))
  expect_silent(validate_evidence(ev))
})

test_that("an ItemDef without an OID triggers a partial-parse warning", {
  path <- write_define(
    '  <def:ItemGroupDef OID="IG.ADSL" Name="ADSL" def:Label="Subject Level">
       <ItemRef ItemOID="IT.ADSL.AGE" Mandatory="Yes"/>
     </def:ItemGroupDef>
     <ItemDef Name="AGE" DataType="integer">
       <Description><TranslatedText>Age</TranslatedText></Description>
     </ItemDef>')

  expect_warning(
    suppressMessages(define_xml_to_evidence(path, ctx)),
    "lack an OID"
  )
})
