# Evidence Schema Version

The evidence table carries a schema version so a table written by one
release of the ecosystem can be recognized, validated, and migrated by
another. `evidence_schema_version()` with no argument returns the
version this release writes. Given an evidence table, it returns the
version stamped on that table, or `NA` when the table predates
versioning.

## Usage

``` r
evidence_schema_version(x = NULL)
```

## Arguments

- x:

  Optional. An evidence data.frame. When supplied, the stamped version
  of that table is returned instead of the current package version.

## Value

A single version string, or `NA_character_` for an unstamped table.

## See also

[`migrate_evidence()`](https://r4sub.github.io/r4subcore/reference/migrate_evidence.md)
to upgrade an older table.

## Examples

``` r
evidence_schema_version()
#> [1] "1.0.0"

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
evidence_schema_version(ev)
#> [1] "1.0.0"
```
