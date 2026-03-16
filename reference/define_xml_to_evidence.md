# Parse Define-XML to Evidence

Reads a Define-XML 2.0/2.1 file and extracts dataset, variable, and
derivation completeness checks into the standard evidence table format.
Three indicators are evaluated for each asset:

- Q-DEFINE-001:

  Dataset is present and has a non-empty label.

- Q-DEFINE-002:

  Variable is documented (has label and dataType).

- Q-DEFINE-003:

  Derivation text is present for derived variables.

## Usage

``` r
define_xml_to_evidence(file, ctx, source_version = "2.1")
```

## Arguments

- file:

  Character. Path to a Define-XML file (`.xml`).

- ctx:

  An
  [r4sub_run_context](https://r4sub.github.io/r4subcore/reference/r4sub_run_context.md)
  providing run and study metadata.

- source_version:

  Character. Version label for the Define-XML standard. Default:
  `"2.1"`.

## Value

A data.frame conforming to the evidence schema, one row per
dataset-level check (Q-DEFINE-001), per variable check (Q-DEFINE-002),
and per derivation check (Q-DEFINE-003).

## Examples

``` r
# \donttest{
# Build a minimal Define-XML 2.1 document
xml_txt <- '<?xml version="1.0" encoding="UTF-8"?>
<ODM xmlns="http://www.cdisc.org/ns/odm/v1.3"
     xmlns:def="http://www.cdisc.org/ns/def/v2.1">
  <Study OID="STUDY001">
    <MetaDataVersion OID="MDV.001" Name="Define-XML 2.1">
      <def:ItemGroupDef OID="IG.ADSL" Name="ADSL" SASDatasetName="ADSL"
                        Repeating="No" Purpose="Analysis"
                        def:Label="Subject-Level Analysis Dataset">
        <ItemRef ItemOID="IT.ADSL.USUBJID" Mandatory="Yes"/>
        <ItemRef ItemOID="IT.ADSL.AGE" Mandatory="Yes"/>
      </def:ItemGroupDef>
      <ItemDef OID="IT.ADSL.USUBJID" Name="USUBJID"
               DataType="text" Length="20"
               SASFieldName="USUBJID">
        <Description><TranslatedText>Unique Subject Identifier</TranslatedText></Description>
      </ItemDef>
      <ItemDef OID="IT.ADSL.AGE" Name="AGE"
               DataType="integer" Length="8"
               SASFieldName="AGE">
        <Description><TranslatedText>Age</TranslatedText></Description>
        <def:Origin Type="Derived">
          <def:Description><TranslatedText>Derived from RFSTDTC</TranslatedText></def:Description>
        </def:Origin>
      </ItemDef>
    </MetaDataVersion>
  </Study>
</ODM>'
tmp <- tempfile(fileext = ".xml")
writeLines(xml_txt, tmp)
ctx <- r4sub_run_context("STUDY001", "DEV")
#> ℹ Run context created: "R4S-20260316112306-vj57qv4a"
ev <- define_xml_to_evidence(tmp, ctx)
#> ℹ Define-XML: found 0 dataset(s) (ItemGroupDef)
#> ℹ Define-XML: found 2 variable definition(s) (ItemDef)
#> ! No ItemGroupDef elements found in Define-XML
#> ℹ define_xml_to_evidence: 3 rows generated
#> ✔ Evidence table created: 3 rows
nrow(ev)
#> [1] 3
# }
```
