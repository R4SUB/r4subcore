# r4subcore

<!-- badges: start -->
[![R-CMD-check](https://github.com/R4SUB/r4subcore/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/R4SUB/r4subcore/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/r4subcore)](https://CRAN.R-project.org/package=r4subcore)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/r4subcore)](https://CRAN.R-project.org/package=r4subcore)
[![r-universe](https://r4sub.r-universe.dev/badges/r4subcore)](https://r4sub.r-universe.dev/r4subcore)
<!-- badges: end -->

**r4subcore** is the foundational package in the **R4SUB** ecosystem. It defines the **core data contracts, parsers, evidence schema, and scoring primitives** needed to quantify clinical submission readiness.

It is intentionally stable: other R4SUB packages (`r4subtrace`, `r4subrisk`, `r4subscore`, `r4subusability`) build on these structures and interfaces.

## Installation

```r
install.packages("r4subcore")
```

Development version:

```r
pak::pak("R4SUB/r4subcore")
```

## Core Concepts

### Evidence Table

All inputs are normalized into a single tabular contract — an **evidence** dataset. This enables scoring, drilldown, traceability, and reporting.

| Column | Type | Description |
|---|---|---|
| `run_id` | chr | Unique ID for a run |
| `study_id` | chr | Study identifier |
| `asset_type` | chr | `dataset`, `define`, `program`, `validation`, `spec`, `other` |
| `asset_id` | chr | Asset identifier (e.g. `ADSL`, `define.xml`) |
| `source_name` | chr | Tool or source name (e.g. `pinnacle21`) |
| `source_version` | chr | Tool version |
| `indicator_id` | chr | Signal definition identifier |
| `indicator_name` | chr | Human-readable indicator name |
| `indicator_domain` | chr | `quality`, `trace`, `risk`, `usability` |
| `severity` | chr | `info`, `low`, `medium`, `high`, `critical` |
| `result` | chr | `pass`, `fail`, `warn`, `na` |
| `metric_value` | dbl | Numeric value (if applicable) |
| `metric_unit` | chr | Unit for metric |
| `message` | chr | Short description |
| `location` | chr | Pointer (dataset / variable / rule) |
| `evidence_payload` | json | Raw structured payload |
| `created_at` | POSIXct | Ingestion timestamp |

### Quick Start

```r
library(r4subcore)

# Create a run context
ctx <- r4sub_run_context(study_id = "ABC123", environment = "DEV")
ctx$run_id

# Coerce raw data into the evidence schema
ev <- as_evidence(raw_df)
validate_evidence(ev)

# Summarize
evidence_summary(ev)
```

## Key Functions

| Function | Purpose |
|---|---|
| `r4sub_run_context()` | Create a reproducible run context |
| `as_evidence()` | Coerce a data frame to the evidence schema |
| `validate_evidence()` | Enforce schema contract |
| `bind_evidence()` | Safely combine evidence from multiple sources |
| `evidence_summary()` | Pass/warn/fail counts by domain |
| `p21_to_evidence()` | Ingest Pinnacle 21 CSV output |
| `define_xml_to_evidence()` | Ingest Define-XML metadata |

## Design Principles

- **Contract-first:** normalize everything into evidence rows
- **Transparent scoring:** no black-box weights; everything configurable
- **Tool-agnostic:** supports P21, Define-XML, and extensible to other sources
- **Reproducible:** `run_id` + `source_version` captured everywhere
- **Composable:** small functions, no tight coupling

## Package Scope

**In scope:** evidence schema, parsers, indicator interfaces, scoring primitives, run context.

**Out of scope:** full SCI calculation (`r4subscore`), dashboards (`r4subui`), traceability logic (`r4subtrace`), risk quantification (`r4subrisk`), usability indicators (`r4subusability`).

## License

MIT
