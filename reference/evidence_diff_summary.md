# Tidy Summary of an Evidence Diff

Rolls an
[`evidence_diff()`](https://r4sub.github.io/r4subcore/reference/evidence_diff.md)
result up into a small, tidy data.frame: how many rows fell into each
`change` category, how many into each `direction`, and a single
net-movement figure.

## Usage

``` r
evidence_diff_summary(diff)
```

## Arguments

- diff:

  A data.frame produced by
  [`evidence_diff()`](https://r4sub.github.io/r4subcore/reference/evidence_diff.md).

## Value

A tidy data.frame with one row per metric and two columns, `metric`
(character) and `value` (numeric). The metrics are `n_rows`; the change
counts `n_added`, `n_removed`, `n_changed`, `n_unchanged`; the direction
counts `n_improved`, `n_regressed`, `n_neutral`; and `net_delta`. The
returned object carries a `"net_delta_note"` attribute restating that
`net_delta` is a diagnostic, not the SCI.

## Details

The net-movement figure, `net_delta`, is the sum of the signed per-row
deltas from
[`evidence_delta()`](https://r4sub.github.io/r4subcore/reference/evidence_delta.md)
(weighted penalty on the `from` side minus the `to` side). A positive
`net_delta` means the submission's weighted penalty load fell overall; a
negative one means it rose.

`net_delta` is a **diagnostic rollup only**. It is a convenient
one-number read on which way a diff moved, not a calibrated readiness
score, and it is **not** the Submission Confidence Index (SCI). Do not
report or compare it as an SCI value; use the ecosystem's SCI machinery
for that.

## See also

[`evidence_diff()`](https://r4sub.github.io/r4subcore/reference/evidence_diff.md)
and
[`evidence_delta()`](https://r4sub.github.io/r4subcore/reference/evidence_delta.md).

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
evidence_diff_summary(evidence_diff(old, new))
#>        metric value
#> 1      n_rows  3.00
#> 2     n_added  1.00
#> 3   n_removed  1.00
#> 4   n_changed  1.00
#> 5 n_unchanged  0.00
#> 6  n_improved  1.00
#> 7 n_regressed  2.00
#> 8   n_neutral  0.00
#> 9   net_delta  0.25
```
