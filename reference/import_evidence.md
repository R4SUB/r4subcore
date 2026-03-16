# Import Evidence Table from File

Reads an evidence table that was previously saved by
[`export_evidence()`](https://r4sub.github.io/r4subcore/reference/export_evidence.md),
then validates it against the evidence schema.

## Usage

``` r
import_evidence(file, format = c("csv", "rds", "json"))
```

## Arguments

- file:

  Character. Path to the file to read.

- format:

  Character. One of `"csv"`, `"rds"`, or `"json"`. Default: `"csv"`.

## Value

A validated evidence data.frame conforming to the evidence schema.

## Examples

``` r
ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
ev <- suppressMessages(as_evidence(
  data.frame(
    asset_type = "validation", asset_id = "ADSL",
    source_name = "pinnacle21", indicator_id = "SD0001",
    indicator_name = "SD0001", indicator_domain = "quality",
    severity = "high", result = "fail",
    stringsAsFactors = FALSE
  ),
  ctx = ctx
))
tmp_rds <- tempfile(fileext = ".rds")
suppressMessages(export_evidence(ev, tmp_rds, format = "rds"))
ev2 <- suppressMessages(import_evidence(tmp_rds, format = "rds"))
nrow(ev2)
#> [1] 1
```
