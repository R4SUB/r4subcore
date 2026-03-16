# Parse Pinnacle21 Output to Evidence

Converts a data.frame of Pinnacle21-style validation results into the
standard evidence table format. Column names are detected
case-insensitively.

## Usage

``` r
p21_to_evidence(
  p21_df,
  ctx,
  asset_type = "validation",
  source_version = NULL,
  default_domain = "quality"
)
```

## Arguments

- p21_df:

  A data.frame containing Pinnacle21 validation output. Expected columns
  (case-insensitive): `Rule` (or `Rule ID`), `Message`, `Severity`,
  `Dataset`, `Variable`, `Result` (or `Status`).

- ctx:

  A
  [r4sub_run_context](https://r4sub.github.io/r4subcore/reference/r4sub_run_context.md)
  providing run and study metadata.

- asset_type:

  Character. Asset type label. Default: `"validation"`.

- source_version:

  Character or `NULL`. Version of the P21 tool.

- default_domain:

  Character. Indicator domain. Default: `"quality"`.

## Value

A data.frame conforming to the evidence schema.

## Examples

``` r
p21_raw <- data.frame(
  Rule = c("SD0001", "SD0002"),
  Message = c("Missing variable label", "Invalid format"),
  Severity = c("Error", "Warning"),
  Dataset = c("ADSL", "ADAE"),
  Variable = c("AGE", "AESTDTC"),
  Status = c("Failed", "Warning"),
  stringsAsFactors = FALSE
)
ctx <- r4sub_run_context("STUDY1", "DEV")
#> ℹ Run context created: "R4S-20260316101039-6glbznel"
ev <- p21_to_evidence(p21_raw, ctx)
#> ℹ Parsed 2 rows from Pinnacle21 output
#> ✔ Evidence table created: 2 rows
```
