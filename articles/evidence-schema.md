# The R4SUB Evidence Schema

Every package in the R4SUB ecosystem reads and writes the same evidence
table. A quality tool, a traceability engine, and a risk model all emit
rows in this one shape, which is what lets `r4subscore` combine them
into a single readiness score. This article is the formal specification
of that table.

``` r

library(r4subcore)
```

## The columns

[`evidence_schema_spec()`](https://r4sub.github.io/r4subcore/reference/evidence_schema_spec.md)
returns the schema as a table: one row per column, its type, whether it
is required, its controlled vocabulary if it has one, and a short
description.

``` r

spec <- evidence_schema_spec()
knitr::kable(spec)
```

| column | type | required | allowed_values | description |
|:---|:---|:---|:---|:---|
| run_id | character | TRUE | NA | Identifier for the run that produced the evidence. |
| study_id | character | TRUE | NA | Study the evidence belongs to. |
| asset_type | character | TRUE | dataset, define, program, validation, spec, other | Kind of asset the check was run against. |
| asset_id | character | TRUE | NA | Identifier of the asset, for example a dataset name. |
| source_name | character | TRUE | NA | Tool or process that produced the evidence. |
| source_version | character | FALSE | NA | Version of the source tool or process. |
| indicator_id | character | TRUE | NA | Stable identifier for the check or indicator. |
| indicator_name | character | TRUE | NA | Human-readable name of the indicator. |
| indicator_domain | character | TRUE | quality, trace, risk, usability | Readiness pillar the indicator contributes to. |
| severity | character | TRUE | info, low, medium, high, critical | How serious a failure of this check is. |
| result | character | TRUE | pass, fail, warn, na | Outcome of the check. |
| metric_value | double | FALSE | NA | Numeric value behind the result, when there is one. |
| metric_unit | character | FALSE | NA | Unit for metric_value. |
| message | character | FALSE | NA | Free-text explanation of the result. |
| location | character | FALSE | NA | Where the finding sits, for example a variable or file. |
| evidence_payload | character | FALSE | NA | JSON payload with the raw detail behind the result. |
| created_at | POSIXct | TRUE | NA | Time the evidence row was created. |

A row is one observation: a single check, run against a single asset,
producing a single result. Required columns must be present and
non-missing. Nullable columns may be `NA`.

## Controlled vocabularies

Four columns are restricted to a fixed set of values. Using the
canonical set keeps scoring and aggregation consistent across sources.

``` r

schema <- evidence_schema()
schema$asset_type$allowed
#> [1] "dataset"    "define"     "program"    "validation" "spec"      
#> [6] "other"
schema$indicator_domain$allowed
#> [1] "quality"   "trace"     "risk"      "usability"
schema$severity$allowed
#> [1] "info"     "low"      "medium"   "high"     "critical"
schema$result$allowed
#> [1] "pass" "fail" "warn" "na"
```

`severity` runs from `info` (no weight) to `critical` (full weight).
`indicator_domain` names the readiness pillar the check feeds: quality,
trace, risk, or usability. `result` is the outcome, where `na` means the
check did not apply.

If your source uses different labels,
[`canon_severity()`](https://r4sub.github.io/r4subcore/reference/canon_severity.md)
and
[`canon_result()`](https://r4sub.github.io/r4subcore/reference/canon_result.md)
map common aliases onto the canonical set:

``` r

canon_severity(c("HIGH", "warning", "Error"))
#> [1] "high"   "medium" "high"
canon_result(c("PASS", "Failed", "N/A"))
#> [1] "pass" "fail" "na"
```

## Run context

`run_id` and `study_id` tie every row back to the run that produced it.
Rather than set them by hand on each row, create a run context once and
let
[`as_evidence()`](https://r4sub.github.io/r4subcore/reference/as_evidence.md)
fill them in:

``` r

ctx <- r4sub_run_context(study_id = "STUDY01", environment = "DEV")
#> ℹ Run context created: "R4S-20261025204223-wl4dieex"

ev <- as_evidence(
  data.frame(
    asset_type = "validation",
    asset_id = "ADSL",
    source_name = "pinnacle21",
    indicator_id = "SD0001",
    indicator_name = "Missing variable",
    indicator_domain = "quality",
    severity = "high",
    result = "fail",
    message = "Variable AGEU is missing",
    stringsAsFactors = FALSE
  ),
  ctx = ctx
)
#> ✔ Evidence table created: 1 row

ev[, c("run_id", "study_id", "indicator_id", "severity", "result")]
#>                        run_id study_id indicator_id severity result
#> 1 R4S-20261025204223-wl4dieex  STUDY01       SD0001     high   fail
```

## Validation

[`as_evidence()`](https://r4sub.github.io/r4subcore/reference/as_evidence.md)
validates as it builds, and
[`validate_evidence()`](https://r4sub.github.io/r4subcore/reference/validate_evidence.md)
checks an existing table against the specification: required columns
present, types correct, and controlled vocabularies respected. A table
that passes
[`validate_evidence()`](https://r4sub.github.io/r4subcore/reference/validate_evidence.md)
is guaranteed to score.

``` r

validate_evidence(ev)
```

Because the specification is a function, downstream packages can rely on
it directly rather than restating the column list. That is what keeps
the ecosystem in step: there is one schema, and this is it.

## Schema versioning

The schema will change over time: a new column, a widened vocabulary. So
that a table written by one release can be recognized by another, every
evidence table carries a schema version.
[`as_evidence()`](https://r4sub.github.io/r4subcore/reference/as_evidence.md)
stamps it, and
[`evidence_schema_version()`](https://r4sub.github.io/r4subcore/reference/evidence_schema_version.md)
reports it.

``` r

evidence_schema_version()
#> [1] "1.0.0"
evidence_schema_version(ev)
#> [1] "1.0.0"
```

[`validate_evidence()`](https://r4sub.github.io/r4subcore/reference/validate_evidence.md)
reads the stamp. An unstamped table is tolerated, because a table can
predate versioning or lose its stamp on a round-trip through CSV or
JSON. A table stamped with a version this release does not recognize,
including a version newer than the installed package, is refused rather
than scored on guesswork. When a table is stamped with an older, known
version,
[`migrate_evidence()`](https://r4sub.github.io/r4subcore/reference/migrate_evidence.md)
upgrades it to the current schema:

``` r

migrated <- migrate_evidence(ev)
evidence_schema_version(migrated)
#> [1] "1.0.0"
```

The policy is deliberately asymmetric: read older tables by migrating
them forward, but never pretend to understand a newer one. That is what
lets the ecosystem move without breaking tables already written.
