# Validate Evidence Table

Checks that a data.frame conforms to the evidence schema. Verifies
column presence, types, and controlled vocabulary values.

## Usage

``` r
validate_evidence(ev)
```

## Arguments

- ev:

  A data.frame to validate.

## Value

`TRUE` invisibly if valid; throws an error otherwise.

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
validate_evidence(ev)
```
