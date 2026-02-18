## Resubmission (Round 2)

Addressing all three points raised by the CRAN reviewer:

### 1. References in Description

There are no published academic papers or external references that describe
the methods implemented in this package. The evidence schema, severity weight
mappings, normalization and aggregation primitives are original work designed
specifically for this package. No doi, ISBN, or URL references are applicable.

### 2. \dontrun{} replaced

All four `\dontrun{}` wrappers have been replaced with fully self-contained,
runnable examples. Each example constructs the required evidence table inline
and completes in well under 5 seconds:

- `validate_evidence()` — example builds and validates a minimal evidence table
- `bind_evidence()` — example creates two tables and row-binds them
- `evidence_summary()` — example builds a table and summarises it
- `aggregate_indicator_score()` — example builds a multi-row table and scores it

### 3. Copyright holder in Authors@R

The LICENSE file previously listed "R4SUB Contributors" as the copyright holder,
which was not reflected in `Authors@R`. Fixed:

- LICENSE now names "Pawan Rama Mali" as the copyright holder
- Authors@R now includes the `cph` (copyright holder) role for Pawan Rama Mali
  alongside the existing `aut` and `cre` roles

### 4. Acronyms

The following acronym appears in the Description field and has been expanded
on first use:

- **R4SUB**: now written as "R4SUB (R for Regulatory Submission)" on first
  mention in the Description field. This is the name of the broader open-source
  ecosystem this package belongs to; it is not a pre-existing industry standard
  abbreviation.

No other non-obvious acronyms appear in the Title or Description fields.

---

## Test environments

- Windows 11 (local), R 4.5.2
- devtools::check() result: 0 errors | 0 warnings | 0 notes

## R CMD check results

0 errors | 0 warnings | 0 notes
