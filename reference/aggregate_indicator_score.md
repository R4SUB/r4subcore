# Aggregate Indicator Scores

Computes summary scores from an evidence table, grouped by one or more
columns.

## Usage

``` r
aggregate_indicator_score(
  ev,
  by = "indicator_id",
  method = c("mean", "min", "weighted")
)
```

## Arguments

- ev:

  A valid evidence data.frame.

- by:

  Character vector of column names to group by. Default:
  `c("indicator_id")`.

- method:

  Aggregation method: `"mean"`, `"min"`, or `"weighted"`. The
  `"weighted"` method uses
  [`severity_to_weight()`](https://r4sub.github.io/r4subcore/reference/severity_to_weight.md)
  and
  [`result_to_score()`](https://r4sub.github.io/r4subcore/reference/result_to_score.md).

## Value

A data.frame with grouping columns plus `score` (0–1) and `n_evidence`
(count of rows).

## Examples

``` r
ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
ev <- suppressMessages(as_evidence(
  data.frame(
    asset_type = rep("validation", 3), asset_id = rep("ADSL", 3),
    source_name = rep("pinnacle21", 3),
    indicator_id = c("SD0001", "SD0001", "SD0002"),
    indicator_name = c("SD0001", "SD0001", "SD0002"),
    indicator_domain = rep("quality", 3),
    severity = c("high", "medium", "low"),
    result = c("fail", "warn", "pass"),
    stringsAsFactors = FALSE
  ),
  ctx = ctx
))
aggregate_indicator_score(ev, by = "indicator_id", method = "weighted")
#>   indicator_id score n_evidence
#> 1       SD0001   0.2          2
#> 2       SD0002   1.0          1
```
