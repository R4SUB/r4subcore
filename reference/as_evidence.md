# Coerce to Evidence Table

Takes a data.frame and coerces it into a valid evidence table. Fills in
missing nullable columns with `NA` of the correct type and validates
controlled vocabulary columns.

## Usage

``` r
as_evidence(x, ctx = NULL, ...)
```

## Arguments

- x:

  A data.frame (or tibble) with at least the required evidence columns.

- ctx:

  An optional
  [r4sub_run_context](https://r4sub.github.io/r4subcore/reference/r4sub_run_context.md).
  If provided, `run_id` and `study_id` are filled from the context when
  missing.

- ...:

  Additional columns to set (e.g., `asset_type = "validation"`).

## Value

A data.frame conforming to the evidence schema.

## Examples

``` r
ctx <- r4sub_run_context("STUDY1", "DEV")
#> ℹ Run context created: "R4S-20260316112306-ombeu7fd"
df <- data.frame(
  asset_type = "validation",
  asset_id = "ADSL",
  source_name = "pinnacle21",
  indicator_id = "P21-001",
  indicator_name = "Missing variable",
  indicator_domain = "quality",
  severity = "high",
  result = "fail",
  message = "Variable AGEU missing",
  stringsAsFactors = FALSE
)
ev <- as_evidence(df, ctx = ctx)
#> ✔ Evidence table created: 1 row
```
