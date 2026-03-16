# Getting Started with r4subcore

``` r
library(r4subcore)
```

## Overview

`r4subcore` is the foundation of the R4SUB (R for Regulatory Submission)
ecosystem. It defines three responsibilities that every other R4SUB
pillar package depends on:

1.  **Run context** — a lightweight metadata envelope that ties every
    piece of evidence back to a specific study and execution event.
2.  **Evidence schema** — a fixed 17-column contract that standardises
    how findings from heterogeneous sources (validators, parsers, manual
    checks) are stored and exchanged.
3.  **Validation helpers** — functions that enforce the schema at every
    ingestion point so downstream scoring and profiling can always trust
    the data.

## Creating a Run Context

A *run context* captures who ran the assessment, on which study, in
which environment, and at what time. Every evidence row you create is
stamped with the `run_id` and `study_id` from its context, ensuring full
traceability.

``` r
ctx <- r4sub_run_context(study_id = "STUDY001", environment = "DEV")
#> ℹ Run context created: "R4S-20260316112310-wl4dieex"
print(ctx)
#> <r4sub_run_context>
#>   run_id:       R4S-20260316112310-wl4dieex 
#>   study_id:     STUDY001 
#>   environment:  DEV 
#>   user:         runner 
#>   created_at:   2026-03-16 11:23:10
```

`run_id` is generated automatically from the timestamp, but you can
supply your own if you need reproducibility in tests or pipelines.

``` r
ctx_custom <- r4sub_run_context(
  study_id    = "STUDY001",
  environment = "PROD",
  run_id      = "RUN-2024-001"
)
#> ℹ Run context created: "RUN-2024-001"
ctx_custom$run_id
#> [1] "RUN-2024-001"
```

## Understanding the Evidence Schema

The evidence schema defines the 17 columns that every evidence table
must contain. Call
[`evidence_schema()`](https://r4sub.github.io/r4subcore/reference/evidence_schema.md)
to inspect the contract at any time:

``` r
schema <- evidence_schema()
# Column names in canonical order
names(schema)
#>  [1] "run_id"           "study_id"         "asset_type"       "asset_id"        
#>  [5] "source_name"      "source_version"   "indicator_id"     "indicator_name"  
#>  [9] "indicator_domain" "severity"         "result"           "metric_value"    
#> [13] "metric_unit"      "message"          "location"         "evidence_payload"
#> [17] "created_at"
```

| Column             | Type      | Required | Notes                                                     |
|--------------------|-----------|----------|-----------------------------------------------------------|
| `run_id`           | character | yes      | Set from run context                                      |
| `study_id`         | character | yes      | Set from run context                                      |
| `asset_type`       | character | yes      | One of: dataset, define, program, validation, spec, other |
| `asset_id`         | character | yes      | e.g. “ADSL”, “define.xml”                                 |
| `source_name`      | character | yes      | Tool or package that produced the finding                 |
| `source_version`   | character | nullable | Version of the source tool                                |
| `indicator_id`     | character | yes      | e.g. “P21-001”, “U-001”                                   |
| `indicator_name`   | character | yes      | Human-readable indicator name                             |
| `indicator_domain` | character | yes      | One of: quality, trace, risk, usability                   |
| `severity`         | character | yes      | One of: info, low, medium, high, critical                 |
| `result`           | character | yes      | One of: pass, fail, warn, na                              |
| `metric_value`     | double    | nullable | Numeric score (0–1 scale typical)                         |
| `metric_unit`      | character | nullable | e.g. “score”, “proportion”, “count”                       |
| `message`          | character | nullable | Human-readable finding description                        |
| `location`         | character | nullable | e.g. “ADSL.USUBJID”                                       |
| `evidence_payload` | character | nullable | JSON string for extended detail                           |
| `created_at`       | POSIXct   | yes      | Set automatically if omitted                              |

### Controlled vocabulary helpers

[`canon_severity()`](https://r4sub.github.io/r4subcore/reference/canon_severity.md)
and
[`canon_result()`](https://r4sub.github.io/r4subcore/reference/canon_result.md)
normalise common aliases to the canonical values accepted by the schema:

``` r
canon_severity(c("ERROR", "warning", "Minor", "CRITICAL"))
#> [1] "high"     "medium"   "low"      "critical"
canon_result(c("PASS", "Failed", "Warning", "N/A"))
#> [1] "pass" "fail" "warn" "na"
```

## Building Evidence with `as_evidence()`

[`as_evidence()`](https://r4sub.github.io/r4subcore/reference/as_evidence.md)
is the main ingestion function. You supply a data frame that contains at
minimum the required columns, pass a run context, and the function:

- fills `run_id` and `study_id` from the context,
- fills nullable columns with appropriately-typed `NA`,
- sets `created_at` to the current time if absent,
- validates the result before returning it.

``` r
raw <- data.frame(
  asset_type       = "validation",
  asset_id         = "ADSL",
  source_name      = "pinnacle21",
  indicator_id     = "P21-SD0001",
  indicator_name   = "Missing variable label",
  indicator_domain = "quality",
  severity         = "high",
  result           = "fail",
  message          = "Variable AGEU is missing a label",
  location         = "ADSL.AGEU",
  metric_value     = 0,
  metric_unit      = "score",
  stringsAsFactors = FALSE
)

ev <- as_evidence(raw, ctx = ctx)
#> ✔ Evidence table created: 1 row
```

You can inspect the resulting evidence table:

``` r
# All 17 schema columns are present
ncol(ev)
#> [1] 17
ev[, c("run_id", "study_id", "indicator_id", "result", "severity")]
#>                        run_id study_id indicator_id result severity
#> 1 R4S-20260316112310-wl4dieex STUDY001   P21-SD0001   fail     high
```

## Validating Evidence

[`validate_evidence()`](https://r4sub.github.io/r4subcore/reference/validate_evidence.md)
runs the same checks that
[`as_evidence()`](https://r4sub.github.io/r4subcore/reference/as_evidence.md)
calls internally. Use it when you receive evidence produced externally
and want to confirm it meets the contract before processing:

``` r
validate_evidence(ev)  # returns TRUE invisibly if everything is valid
```

## Binding Multiple Evidence Tables

When combining evidence from different sources or indicators, use
[`bind_evidence()`](https://r4sub.github.io/r4subcore/reference/bind_evidence.md).
It validates each table individually before combining, preventing schema
violations from silently propagating:

``` r
# A second finding — a passed check on the same dataset
raw2 <- data.frame(
  asset_type       = "dataset",
  asset_id         = "ADSL",
  source_name      = "r4subcore",
  indicator_id     = "Q-NROW-001",
  indicator_name   = "Dataset row count",
  indicator_domain = "quality",
  severity         = "info",
  result           = "pass",
  message          = "ADSL has 254 subjects",
  metric_value     = 254,
  metric_unit      = "count",
  stringsAsFactors = FALSE
)
ev2 <- as_evidence(raw2, ctx = ctx)
#> ✔ Evidence table created: 1 row

combined <- bind_evidence(ev, ev2)
#> ✔ Bound 2 evidence tables: 2 total rows
nrow(combined)
#> [1] 2
```

## Quick Overview with `evidence_summary()`

[`evidence_summary()`](https://r4sub.github.io/r4subcore/reference/evidence_summary.md)
aggregates an evidence table by domain, severity, result, and source,
giving a one-page digest of the findings:

``` r
evidence_summary(combined)
#>   indicator_domain severity result source_name n
#> 1          quality     high   fail  pinnacle21 1
#> 2          quality     info   pass   r4subcore 1
```

## Exporting and Importing Evidence

Evidence tables can be persisted and reloaded in CSV, RDS, or JSON
format. The exported file retains the full schema so
[`import_evidence()`](https://r4sub.github.io/r4subcore/reference/import_evidence.md)
can re-validate it on the way back in.

``` r
# Export to CSV
tmp <- tempfile(fileext = ".csv")
export_evidence(combined, file = tmp, format = "csv")

# Import and re-validate
ev_reloaded <- import_evidence(tmp, format = "csv")
nrow(ev_reloaded)
```

RDS is the most faithful format because it preserves POSIXct without any
string-conversion round-trip. JSON is useful when evidence needs to be
consumed by non-R tooling.

## What’s Next

Once you have an evidence table, the other R4SUB pillar packages consume
it directly:

- **r4subusability** — four usability indicators (label quality,
  Define-XML completeness, annotation coverage, reviewer guide
  presence).
- **r4subscore** — weighted scoring across all domains into a single
  submission readiness index.
- **r4subprofile** — regulatory authority profile templates (FDA, EMA,
  PMDA, ANVISA, Health Canada, NMPA) that map scores to submission
  requirements.
