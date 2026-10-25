# Migrate an Evidence Table to a Newer Schema Version

Upgrades an evidence table written by an older release to the requested
schema version, applying each intervening migration step in order. A
table with no stamp is treated as the earliest known version. A table
already at the target is returned unchanged apart from the version
stamp.

## Usage

``` r
migrate_evidence(x, to = current_evidence_schema_version())
```

## Arguments

- x:

  An evidence data.frame.

- to:

  Target schema version. Defaults to the current version.

## Value

The migrated evidence data.frame, stamped with `to`.

## See also

[`evidence_schema_version()`](https://r4sub.github.io/r4subcore/reference/evidence_schema_version.md).

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
ev <- migrate_evidence(ev)
evidence_schema_version(ev)
#> [1] "1.0.0"
```
