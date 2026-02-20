# r4subcore

[![R-CMD-check](https://github.com/R4SUB/r4subcore/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/R4SUB/r4subcore/actions/workflows/R-CMD-check.yaml) [![CRAN status](https://www.r-pkg.org/badges/version/r4subcore)](https://CRAN.R-project.org/package=r4subcore) [![CRAN downloads](https://cranlogs.r-pkg.org/badges/r4subcore)](https://CRAN.R-project.org/package=r4subcore) [![r-universe](https://r4sub.r-universe.dev/badges/r4subcore)](https://r4sub.r-universe.dev/r4subcore)

**r4subcore** is the foundational package in the **R4SUB** ecosystem. It defines the **core data contracts, parsers, evidence schema, and scoring primitives** needed to quantify *clinical submission readiness*.

It is intentionally "boring and stable": other R4SUB packages (e.g., `r4subtrace`, `r4subrisk`, `r4subscore`) build on these structures and interfaces.

---

## Why r4subcore?

Clinical submission readiness is rarely a single tool output. It's an **evidence graph** across:

- SDTM/ADaM datasets and metadata
- Define.xml / ARM / reviewer guides
- Validation results (Pinnacle21, OpenCDISC, internal rule engines)
- Traceability / derivations (ADaM spec <-> code <-> outputs)
- Usability / reviewer experience signals

**r4subcore** provides:
1. A standardized **Evidence Table** schema
2. Common **parsers** to ingest heterogeneous sources
3. A consistent **indicator / signal** abstraction
4. **Scoring primitives** (normalize, weight, calibrate, aggregate)
5. A reproducible **run context** (run_id, dataset_id, study_id, tool version)

---

## Package scope

### In scope
- Evidence schema + validation
- Parsers for common sources (initial focus on P21-style outputs + define.xml scaffolding)
- Common utilities: ID generation, severity mapping, controlled terminology mapping, standard columns
- Indicator interfaces (how other packages implement signals)
- Transparent scoring components (no hidden "magic")

### Out of scope
- Full SCI (Submission Confidence Index) calculation (belongs in `r4subscore`)
- End-to-end dashboards / Shiny apps (belongs in `r4subui`)
- Full traceability logic (belongs in `r4subtrace`)
- Domain-specific oncology rules (belongs in extension packages)

---

## Installation

### CRAN
```r
install.packages("r4subcore")
```

### Development install
```r
# install.packages("pak")
pak::pak("R4SUB/r4subcore")
```

### Requirements

* R >= 4.2
* Suggested: `arrow`, `xml2`, `dplyr`, `readr`, `jsonlite`, `cli`

---

## Core concepts

### 1) Evidence Table (the heart of R4SUB)

All inputs are normalized into a single tabular contract: an **evidence** dataset.
This enables scoring, drilldown, traceability, and reporting.

**Minimum columns (v0.1):**

| column             | type    | meaning                                                    |
| ------------------ | ------- | ---------------------------------------------------------- |
| `run_id`           | chr     | unique ID for a run                                        |
| `study_id`         | chr     | study identifier                                           |
| `asset_type`       | chr     | `dataset`, `define`, `program`, `validation`, `spec`, etc. |
| `asset_id`         | chr     | unique ID of the asset (e.g., `ADSL`, `define.xml`)        |
| `source_name`      | chr     | tool/source name (e.g., `pinnacle21`)                      |
| `source_version`   | chr     | tool version                                               |
| `indicator_id`     | chr     | the signal definition identifier                           |
| `indicator_name`   | chr     | human name                                                 |
| `indicator_domain` | chr     | `quality`, `trace`, `risk`, `usability`                    |
| `severity`         | chr     | `info`, `low`, `medium`, `high`, `critical`                |
| `result`           | chr     | `pass`, `fail`, `warn`, `na`                               |
| `metric_value`     | dbl     | numeric value (if applicable)                              |
| `metric_unit`      | chr     | unit for metric                                            |
| `message`          | chr     | short description                                          |
| `location`         | chr     | pointer (dataset/variable/rule line)                       |
| `evidence_payload` | json    | raw structured payload                                     |
| `created_at`       | POSIXct | ingestion timestamp                                        |

**Guarantees:**

* Each row is a single unit of evidence
* Evidence is immutable (append-only semantics recommended)
* Score consumers can rely on consistent meaning

Use:

* `as_evidence()` to coerce raw data
* `validate_evidence()` to enforce contract
* `bind_evidence()` to combine sources safely

---

### 2) Indicators (signals)

An **indicator** is a definition of *what to measure*, not necessarily how to calculate it.

Indicators have:

* `indicator_id` (stable)
* `domain` (quality/trace/risk/usability)
* `description`
* `expected_inputs` (evidence sources required)
* `default_thresholds`
* optional `tags` (e.g., `define`, `adam`, `sdtm`, `spec`)

`r4subcore` provides:

* indicator registry helpers (local registry first, remote later)
* validation to ensure indicator metadata is well-formed

Other packages implement the actual calculations and output evidence rows using these IDs.

---

### 3) Scoring primitives (transparent & composable)

`r4subcore` includes **small, auditable** functions for:

* mapping severity -> numeric penalty
* normalizing metrics to 0-1
* applying weights
* aggregating evidence into indicator scores

SCI itself is not in this package.

---

## Quick start

### Create a run context

```r
library(r4subcore)

ctx <- r4sub_run_context(
  study_id = "ABC123",
  environment = "DEV",
  user = Sys.info()[["user"]]
)
ctx$run_id
```

### Ingest validation results (example)

```r
raw <- read.csv("p21_report.csv")

ev <- p21_to_evidence(
  raw,
  ctx = ctx,
  asset_type = "validation",
  source_version = "P21-3.0"
)

validate_evidence(ev)
```

### Summarize evidence quickly

```r
evidence_summary(ev)
```

---

## Architecture

### Main modules

* `R/evidence_schema.R` -- schema + validators
* `R/run_context.R` -- run metadata
* `R/parsers_p21.R` -- Pinnacle21 ingestion (first parser)
* `R/indicators.R` -- indicator metadata + registry
* `R/scoring_primitives.R` -- severity mapping, normalization, aggregation
* `R/utils_ids.R` -- ID helpers, hashing
* `R/utils_json.R` -- JSON payload helpers

### Extensibility

* New parsers should output evidence via `as_evidence()`
* New indicators should register IDs and domain metadata
* Consumers should never depend on tool-specific raw formats

---

## Design principles

* **Contract-first:** normalize everything into evidence rows
* **Transparent scoring:** no black-box weights; everything configurable
* **Tool-agnostic:** support P21 now, but leave room for OpenCDISC, internal engines
* **Reproducible:** run_id + source_version captured everywhere
* **Composable:** small functions, no tight coupling

---

## Roadmap

### v0.1

* Evidence schema + validator
* Run context
* P21-style parser (CSV/XLSX minimal)
* Indicator metadata helpers
* Severity -> numeric mappings
* Minimal summarizers

### v0.2

* define.xml ingestion (structure-level metadata)
* Arrow/parquet IO
* Evidence "joins" (dataset <-> variable <-> rule)
* Config profiles (e.g., `FDA_ADaM_basic`, `EMA_SDTM_basic`)

---

## Contributing

* Use `devtools::check()` before PR
* Add tests for each parser and scoring function
* Do not break evidence schema without a version bump + migration note

---

## License

MIT -- see [LICENSE](LICENSE) file.
