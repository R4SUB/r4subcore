# Bind Evidence Tables

Combines several R4SUB evidence data.frames – for example the outputs of
different adapters or indicators across the ecosystem – into one
validated evidence table. Each input is validated against the evidence
schema, the tables are row-bound, and colliding rows (more than one row
sharing the same key) are detected and reported. The combined table
carries a collision report in its `evidence_collisions` attribute, and
duplicates can optionally be resolved.

## Usage

``` r
bind_evidence(
  ...,
  keys = c("indicator_id", "asset_id", "location"),
  on_duplicate = c("keep", "first", "last", "error")
)
```

## Arguments

- ...:

  Evidence data.frames to bind, or a single list of them.

- keys:

  Character vector of columns forming the collision key, passed to
  [`evidence_collisions()`](https://r4sub.github.io/r4subcore/reference/evidence_collisions.md).
  Defaults to `c("indicator_id", "asset_id", "location")`.

- on_duplicate:

  How to handle keys that appear on more than one row: `"keep"`
  (default) keeps all rows, `"first"` keeps the first row on each key,
  `"last"` keeps the last, and `"error"` aborts if any key collides.

## Value

A single combined evidence data.frame with an `evidence_collisions`
attribute holding the collision report (see
[`evidence_collisions()`](https://r4sub.github.io/r4subcore/reference/evidence_collisions.md)).

## Details

The default `on_duplicate = "keep"` preserves every row, so the combined
table is exactly the row-bind of the inputs; the collision report is
attached either way. Resolution, when requested, keeps the first or last
row on each key, or aborts. This is purely structural: it never scores,
weights, or decides which of two conflicting rows is correct.

## See also

[`evidence_collisions()`](https://r4sub.github.io/r4subcore/reference/evidence_collisions.md).

## Examples

``` r
ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
make_ev <- function(ind_id) {
  suppressMessages(as_evidence(
    data.frame(
      asset_type = "validation", asset_id = "ADSL",
      source_name = "pinnacle21", indicator_id = ind_id,
      indicator_name = ind_id, indicator_domain = "quality",
      severity = "low", result = "pass",
      stringsAsFactors = FALSE
    ),
    ctx = ctx
  ))
}
ev1 <- make_ev("IND-001")
ev2 <- make_ev("IND-002")
combined <- suppressMessages(bind_evidence(ev1, ev2))
nrow(combined)
#> [1] 2
attr(combined, "evidence_collisions")
#> [1] indicator_id asset_id     location     n            n_distinct  
#> [6] conflict     values      
#> <0 rows> (or 0-length row.names)
```
