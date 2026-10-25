# Compare Two Evidence Tables

Reports what changed between two evidence tables, for example the same
submission checked at two points in time. Rows are matched on a key (by
default the indicator applied to a specific asset) and classified as
added, removed, changed, or unchanged, with a direction that says
whether each change made the submission better or worse.

## Usage

``` r
evidence_diff(
  old,
  new,
  key = c("indicator_id", "asset_id"),
  include_unchanged = FALSE
)
```

## Arguments

- old, new:

  Valid evidence data.frames, as produced by
  [`as_evidence()`](https://r4sub.github.io/r4subcore/reference/as_evidence.md).
  `old` is the earlier state and `new` the later one.

- key:

  Character vector of columns that identify a comparable row. Default
  `c("indicator_id", "asset_id")`.

- include_unchanged:

  Logical. If `FALSE` (default) unchanged matched rows are dropped from
  the result.

## Value

A data.frame with the key columns plus `indicator_name`,
`indicator_domain`, `change` (`added`, `removed`, `changed`, or
`unchanged`), `direction` (`improved`, `regressed`, or `neutral`),
`result_from`, `result_to`, `severity_from`, `severity_to`,
`metric_from`, and `metric_to`. Rows are ordered with the most
actionable first: regressions, then new findings, then resolved
findings, then improvements. The returned object carries a `"summary"`
attribute with the count in each `change` category.

## Details

When a key appears on more than one row within a table (an indicator
that emits several messages for the same asset, say), the rows are
collapsed to the worst one for that key before comparison: lowest result
score first (`fail` beats `warn` beats `pass`), then highest severity.
This keeps the diff at the indicator level, which is what a submission
team acts on.

## Examples

``` r
ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
mk <- function(id, result, severity) {
  suppressMessages(as_evidence(
    data.frame(
      asset_type = "validation", asset_id = "ADSL",
      source_name = "pinnacle21", indicator_id = id,
      indicator_name = id, indicator_domain = "quality",
      severity = severity, result = result,
      stringsAsFactors = FALSE
    ),
    ctx = ctx
  ))
}
old <- suppressMessages(bind_evidence(mk("A", "fail", "high"), mk("B", "pass", "low")))
new <- suppressMessages(bind_evidence(mk("A", "pass", "high"), mk("C", "fail", "medium")))
evidence_diff(old, new)
#>   indicator_id asset_id indicator_name indicator_domain  change direction
#> 1            A     ADSL              A          quality changed  improved
#> 2            C     ADSL              C          quality   added regressed
#> 3            B     ADSL              B          quality removed regressed
#>   result_from result_to severity_from severity_to metric_from metric_to
#> 1        fail      pass          high        high          NA        NA
#> 2        <NA>      fail          <NA>      medium          NA        NA
#> 3        pass      <NA>           low        <NA>          NA        NA
```
