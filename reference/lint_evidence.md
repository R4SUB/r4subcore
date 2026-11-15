# Lint an Evidence Table

Checks an evidence table against the R4SUB evidence schema and a handful
of structural best practices, and reports *every* issue it finds at once
as a tidy data.frame. This is the non-fatal counterpart to
[`validate_evidence()`](https://r4sub.github.io/r4subcore/reference/validate_evidence.md):
where
[`validate_evidence()`](https://r4sub.github.io/r4subcore/reference/validate_evidence.md)
throws on the first problem (and is the right gate before scoring),
`lint_evidence()` never throws on a malformed table, so you can see the
full picture of what is wrong in one pass.

## Usage

``` r
lint_evidence(ev, checks = NULL)
```

## Arguments

- ev:

  A data.frame to lint. It need not be a valid evidence table; that is
  the point.

- checks:

  Optional character vector of check codes to run (see the codes in the
  descriptions above). The default, `NULL`, runs them all. Unknown codes
  are rejected.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with one
row per issue and the columns `code` (the check that fired), `level`
(`error`, `warning`, or `note`), `field` (the column the issue concerns,
or `NA` for a table-level issue), `n` (the number of rows affected, or
`NA` for a table-level issue), and `message` (a human-readable
description). A clean table returns a zero-row tibble with those
columns. Rows are ordered most serious first. The result carries a
`"summary"` attribute: a named integer vector counting findings at each
level.

## Details

Every check is structural. The linter reuses the schema from
[`evidence_schema()`](https://r4sub.github.io/r4subcore/reference/evidence_schema.md)
and the version machinery behind
[`evidence_schema_version()`](https://r4sub.github.io/r4subcore/reference/evidence_schema_version.md);
it invents no scoring, weights, thresholds, or controlled vocabularies
of its own, and it never judges whether a finding is good or bad for a
submission. The checks are:

- `error`:

  Issues that would also make
  [`validate_evidence()`](https://r4sub.github.io/r4subcore/reference/validate_evidence.md)
  fail: a required column missing (`missing_required_column`), a schema
  column of the wrong type (`wrong_type`), an `NA` in a non-nullable
  column (`na_in_required`), a value outside a column's controlled
  vocabulary (`invalid_value`), or a schema-version stamp this release
  does not know (`unknown_schema_version`).

- `warning`:

  A schema-version stamp older than the current one
  (`outdated_schema_version`), or an `evidence_payload` cell that is
  neither empty nor valid JSON (`invalid_payload_json`).

- `note`:

  No schema-version stamp at all (`missing_schema_version`); more than
  one row sharing an `(indicator_id, asset_id)` key, which
  [`evidence_diff()`](https://r4sub.github.io/r4subcore/reference/evidence_diff.md)
  would collapse (`duplicate_key`); a `metric_value` with no
  `metric_unit` (`metric_value_without_unit`) or a `metric_unit` with no
  `metric_value` (`metric_unit_without_value`).

## See also

[`validate_evidence()`](https://r4sub.github.io/r4subcore/reference/validate_evidence.md)
for the strict gate, and
[`evidence_schema()`](https://r4sub.github.io/r4subcore/reference/evidence_schema.md)
for the schema the checks are drawn from.

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
# A valid table lints clean.
lint_evidence(ev)
#> # A tibble: 0 × 5
#> # ℹ 5 variables: code <chr>, level <chr>, field <chr>, n <int>, message <chr>

# A hand-built table with problems reports them all at once.
bad <- data.frame(
  run_id = "R1", study_id = "S1", asset_type = "dataset",
  asset_id = "ADSL", source_name = "x", indicator_id = "I1",
  indicator_name = "I1", indicator_domain = "quality",
  severity = "sky-high", result = "pass",
  created_at = Sys.time(), stringsAsFactors = FALSE
)
lint_evidence(bad)
#> # A tibble: 2 × 5
#>   code                   level field        n message                           
#>   <chr>                  <chr> <chr>    <int> <chr>                             
#> 1 invalid_value          error severity     1 Column 'severity' has value(s) ou…
#> 2 missing_schema_version note  NA          NA Evidence table has no schema-vers…
```
