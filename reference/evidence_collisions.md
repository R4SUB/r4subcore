# Detect Colliding Evidence Rows

Finds rows in an evidence table that share the same key and reports
them. By default the key is the `(indicator_id, asset_id, location)`
triple that identifies a single finding on a single place in an asset,
so more than one row on that key is a duplicate that assembling adapters
can introduce. A collision is a *conflict* when the colliding rows
disagree on their outcome, that is when they carry more than one
distinct value in `conflict_on` (`result` by default).

## Usage

``` r
evidence_collisions(
  ev,
  keys = c("indicator_id", "asset_id", "location"),
  conflict_on = "result"
)
```

## Arguments

- ev:

  A valid evidence data.frame.

- keys:

  Character vector of column names forming the collision key. Defaults
  to `c("indicator_id", "asset_id", "location")`.

- conflict_on:

  Single column name whose distinct values decide whether a collision is
  a conflict. Defaults to `"result"`.

## Value

A data.frame with one row per colliding key (keys appearing on more than
one row), ordered by the key columns. It has the `keys` columns plus `n`
(number of rows on the key), `n_distinct` (distinct non-`NA` values of
`conflict_on`), `conflict` (logical, `TRUE` when `n_distinct > 1`), and
`values` (the distinct `conflict_on` values, comma separated). A table
with no collisions yields a zero-row frame with these columns.

## Details

This is purely structural: it groups rows and counts distinct values. It
applies no weights, thresholds, or scoring and makes no judgement about
which row is correct.

## See also

[`bind_evidence()`](https://r4sub.github.io/r4subcore/reference/bind_evidence.md),
which uses this to report and optionally resolve collisions while
combining tables.

## Examples

``` r
ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
ev <- suppressMessages(as_evidence(
  data.frame(
    asset_type = "validation", asset_id = rep("ADSL", 3),
    source_name = c("p21", "p21", "other"),
    indicator_id = c("SD0001", "SD0001", "SD0002"),
    indicator_name = c("SD0001", "SD0001", "SD0002"),
    indicator_domain = "quality", severity = "high",
    result = c("fail", "warn", "pass"), location = c("AGE", "AGE", "SEX"),
    stringsAsFactors = FALSE
  ),
  ctx = ctx
))
evidence_collisions(ev)
#>   indicator_id asset_id location n n_distinct conflict     values
#> 1       SD0001     ADSL      AGE 2          2     TRUE fail, warn
```
