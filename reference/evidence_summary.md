# Summarize Evidence

Returns a summary data.frame with counts grouped by domain, severity,
result, and source.

## Usage

``` r
evidence_summary(ev)
```

## Arguments

- ev:

  A valid evidence data.frame.

## Value

A data.frame with columns: `indicator_domain`, `severity`, `result`,
`source_name`, and `n`.

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
evidence_summary(ev)
#>   indicator_domain severity result source_name n
#> 1          quality     high   fail  pinnacle21 1
```
