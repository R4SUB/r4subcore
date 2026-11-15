# Signed Per-Row Score Delta for an Evidence Diff

Scores each row of an
[`evidence_diff()`](https://r4sub.github.io/r4subcore/reference/evidence_diff.md)
result on a signed scale, so you can see not just *that* a finding moved
but *how much* weight that movement carried. The score reuses the
package's existing scoring primitives,
[`result_to_score()`](https://r4sub.github.io/r4subcore/reference/result_to_score.md)
and
[`severity_to_weight()`](https://r4sub.github.io/r4subcore/reference/severity_to_weight.md);
it invents no new weights.

## Usage

``` r
evidence_delta(diff)
```

## Arguments

- diff:

  A data.frame produced by
  [`evidence_diff()`](https://r4sub.github.io/r4subcore/reference/evidence_diff.md)
  (it must carry the `change`, `direction`, `result_from`, `result_to`,
  `severity_from`, and `severity_to` columns).

## Value

The input `diff`, unchanged in row order and contents, with one new
numeric column `delta` appended: the signed per-row penalty movement
described above. Any `"summary"` attribute on `diff` is preserved.

## Details

Each state (the `from` side and the `to` side of a row) is reduced to a
weighted penalty,
`(1 - result_to_score(result)) * severity_to_weight(severity)`. A
passing check carries no penalty; a failing, high-severity check carries
the most. A row's signed delta is `penalty_from - penalty_to`, so a
**positive** delta means the weighted penalty fell (the submission got
better at that row) and a **negative** delta means it rose. States that
are absent (the missing side of an added or removed row) or `na`
contribute no penalty, so an added finding shows a negative delta and a
resolved finding a positive one.

Note that the delta is penalty-based: adding or dropping a *passing*
check leaves the penalty load unchanged and so scores `0`, even though
[`evidence_diff()`](https://r4sub.github.io/r4subcore/reference/evidence_diff.md)
still labels it `improved` (more coverage) or `regressed` (lost
coverage) in the `direction` column. The two views are complementary:
`direction` is categorical and coverage-aware, the delta is a magnitude.

## See also

[`evidence_diff()`](https://r4sub.github.io/r4subcore/reference/evidence_diff.md)
for the diff this consumes and
[`evidence_diff_summary()`](https://r4sub.github.io/r4subcore/reference/evidence_diff_summary.md)
for the tidy rollup built on top of it.

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
d <- evidence_diff(old, new)
evidence_delta(d)[, c("indicator_id", "change", "direction", "delta")]
#>   indicator_id  change direction delta
#> 1            A changed  improved  0.75
#> 2            C   added regressed -0.50
#> 3            B removed regressed  0.00
```
