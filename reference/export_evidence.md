# Export Evidence Table to File

Validates the evidence table then writes it to disk in the requested
format. Metadata attributes (`exported_at`, `r4subcore_version`, `nrow`)
are attached to the returned path value for traceability.

## Usage

``` r
export_evidence(evidence, file, format = c("csv", "rds", "json"))
```

## Arguments

- evidence:

  A valid evidence data.frame (as produced by
  [`as_evidence()`](https://r4sub.github.io/r4subcore/reference/as_evidence.md)).

- file:

  Character. Destination file path (including extension).

- format:

  Character. One of `"csv"`, `"rds"`, or `"json"`. Default: `"csv"`.

## Value

Invisibly returns `file` with attributes:

- exported_at:

  POSIXct timestamp of export.

- r4subcore_version:

  Package version string.

- nrow:

  Number of rows written.

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
tmp_csv <- tempfile(fileext = ".csv")
out <- suppressMessages(export_evidence(ev, tmp_csv, format = "csv"))
file.exists(out)
#> [1] TRUE
```
