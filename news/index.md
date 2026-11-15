# Changelog

## r4subcore 0.3.1.9000 (development)

- Extend
  [`bind_evidence()`](https://r4sub.github.io/r4subcore/reference/bind_evidence.md)
  into a collision-aware assembler and add
  [`evidence_collisions()`](https://r4sub.github.io/r4subcore/reference/evidence_collisions.md),
  giving downstream packages one validated way to combine evidence from
  several adapters or indicators.
  [`evidence_collisions()`](https://r4sub.github.io/r4subcore/reference/evidence_collisions.md)
  groups an evidence table by the `(indicator_id, asset_id, location)`
  key (the key is configurable) and returns every key that appears on
  more than one row, with the row count, the distinct `result` values,
  and a `conflict` flag that is `TRUE` when those rows disagree.
  [`bind_evidence()`](https://r4sub.github.io/r4subcore/reference/bind_evidence.md)
  now runs that check over the combined table, attaches the report as
  its `evidence_collisions` attribute, and takes `on_duplicate` to keep
  all rows (the default, so existing calls are unchanged), keep the
  first or last row per key, or abort on any collision; it also accepts
  a single list of frames. The logic is purely structural grouping and
  counting – it applies no weights, thresholds, or scoring and never
  decides which conflicting row is correct.
- Add
  [`lint_evidence()`](https://r4sub.github.io/r4subcore/reference/lint_evidence.md),
  a non-fatal counterpart to
  [`validate_evidence()`](https://r4sub.github.io/r4subcore/reference/validate_evidence.md).
  It checks an evidence table against the schema and a few structural
  best practices and returns every issue at once as a tidy
  `code`/`level`/`field`/ `n`/`message` frame, instead of aborting on
  the first problem. The checks are purely structural and reuse the
  existing schema and version machinery: missing required columns, wrong
  column types, `NA` in non-nullable columns, values outside a
  controlled vocabulary, and unknown/outdated/missing schema-version
  stamps, plus notes for unparseable `evidence_payload` JSON, duplicate
  `(indicator_id, asset_id)` keys, and `metric_value`/`metric_unit`
  gaps. It invents no weights, thresholds, or scoring.
- Add
  [`evidence_delta()`](https://r4sub.github.io/r4subcore/reference/evidence_delta.md)
  and
  [`evidence_diff_summary()`](https://r4sub.github.io/r4subcore/reference/evidence_diff_summary.md),
  which extend
  [`evidence_diff()`](https://r4sub.github.io/r4subcore/reference/evidence_diff.md)
  reporting without changing its output.
  [`evidence_delta()`](https://r4sub.github.io/r4subcore/reference/evidence_delta.md)
  appends a signed per-row `delta` to a diff, scoring each change from
  the existing
  [`result_to_score()`](https://r4sub.github.io/r4subcore/reference/result_to_score.md)
  and
  [`severity_to_weight()`](https://r4sub.github.io/r4subcore/reference/severity_to_weight.md)
  primitives as the movement in weighted penalty (positive means the
  penalty fell).
  [`evidence_diff_summary()`](https://r4sub.github.io/r4subcore/reference/evidence_diff_summary.md)
  rolls a diff up into a tidy `metric`/`value` frame with the
  per-category counts and a single `net_delta`. The net-movement figure
  is labelled a diagnostic rollup, not the Submission Confidence Index.
- Add
  [`dataset_variables()`](https://r4sub.github.io/r4subcore/reference/dataset_variables.md),
  which builds the `(dataset, variable)` table that
  [`check_define_consistency()`](https://r4sub.github.io/r4subcore/reference/check_define_consistency.md)
  compares against Define-XML. It reads variable names from a single
  data frame (with a supplied dataset name), a named list of data
  frames, or a directory of `.rds` files (one data frame each); SAS
  transport `.xpt` files are read when the optional `haven` package is
  installed. It only reads names and emits no scoring or evidence of its
  own.
- Fix the “Getting Started” vignette, which still expanded “R4SUB” as “R
  for Regulatory Submission”. It now reads “Ready for Submission”,
  matching the DESCRIPTION and the rest of the ecosystem (corrected in
  0.2.0).
- Add tests for
  [`export_evidence()`](https://r4sub.github.io/r4subcore/reference/export_evidence.md)
  /
  [`import_evidence()`](https://r4sub.github.io/r4subcore/reference/import_evidence.md):
  the JSON round-trip, the traceability attributes on the returned path,
  the missing-file error, and a regression guard for the 0.2.0 fix that
  coerces an all-`NA` character column back to character on import. No
  change to behavior.

## r4subcore 0.3.1

- Metadata and documentation polish for CRAN submission; no user-facing
  changes.

## r4subcore 0.3.0

- Add
  [`evidence_diff()`](https://r4sub.github.io/r4subcore/reference/evidence_diff.md),
  which compares two evidence tables and reports what changed between
  them: which findings were added, removed, or changed, and whether each
  change made the submission better or worse. Rows are matched on the
  indicator and asset, duplicate keys collapse to the worst row, and the
  result is ordered with regressions first so a team sees what to act
  on. This turns the evidence table into something you can track run
  over run, and it is the raw material behind higher-level trend views
  such as `sci_diff()`.

## r4subcore 0.2.1

- [`hash_id()`](https://r4sub.github.io/r4subcore/reference/hash_id.md)
  now hashes in memory with
  [`rlang::hash()`](https://rlang.r-lib.org/reference/hash.html) instead
  of writing a temporary file and calling
  [`tools::md5sum`](https://rdrr.io/r/tools/md5sum.html) on every call.
  The result is the same shape (a 32-character hex string,
  deterministic) but roughly three orders of magnitude faster, which
  removes the main bottleneck when building a trace model or generating
  identifiers for a large submission. The exact hash values change; they
  are opaque identifiers, not a persisted contract.
- Add
  [`check_define_consistency()`](https://r4sub.github.io/r4subcore/reference/check_define_consistency.md)
  and
  [`define_variables()`](https://r4sub.github.io/r4subcore/reference/define_variables.md).
  The first compares the variables declared in Define-XML against the
  variables actually present in the datasets and emits evidence for
  each: a match, a variable documented but absent from the data, or a
  variable present but not documented. Drift between Define-XML and the
  data is a common submission problem, and this makes it a scored
  quality indicator. The second extracts the dataset and variable pairs
  from a Define-XML file.

## r4subcore 0.2.0

- Add
  [`read_p21_report()`](https://r4sub.github.io/r4subcore/reference/read_p21_report.md),
  which loads a Pinnacle 21 validation report from disk (CSV natively,
  Excel through the optional `readxl` package) ready for
  [`p21_to_evidence()`](https://r4sub.github.io/r4subcore/reference/p21_to_evidence.md).
  A team can now go from the report Pinnacle 21 already produces to
  scored evidence without hand-building a table.
- [`canon_severity()`](https://r4sub.github.io/r4subcore/reference/canon_severity.md)
  now recognizes “notice”, the severity Pinnacle 21 uses for its lowest
  level, mapping it onto `info`.
- The evidence table now carries a schema version.
  [`as_evidence()`](https://r4sub.github.io/r4subcore/reference/as_evidence.md)
  stamps it,
  [`evidence_schema_version()`](https://r4sub.github.io/r4subcore/reference/evidence_schema_version.md)
  reports the current version or the version of a given table, and
  [`migrate_evidence()`](https://r4sub.github.io/r4subcore/reference/migrate_evidence.md)
  upgrades a table written by an older release.
  [`validate_evidence()`](https://r4sub.github.io/r4subcore/reference/validate_evidence.md)
  tolerates an unstamped table but refuses an unknown or newer version,
  so a table written by a future release cannot be silently mis-scored.
- Fix
  [`import_evidence()`](https://r4sub.github.io/r4subcore/reference/import_evidence.md):
  a CSV or JSON round-trip could leave an all-`NA` character column (for
  example `source_version`) typed as logical, which then failed
  validation. Every schema column is now coerced back to its declared
  type on import.
- Add
  [`evidence_schema_spec()`](https://r4sub.github.io/r4subcore/reference/evidence_schema_spec.md),
  which publishes the evidence schema as a tidy table (column, type,
  required, allowed values, description) for use as a data dictionary or
  reference.
- Add vignette: “The R4SUB Evidence Schema”, the formal specification of
  the evidence table shared across the ecosystem.
- Add
  [`adrg_to_evidence()`](https://r4sub.github.io/r4subcore/reference/adrg_to_evidence.md)
  and
  [`adrg_sections()`](https://r4sub.github.io/r4subcore/reference/adrg_sections.md)
  for Analysis Data Reviewer’s Guide coverage. Given a structured
  summary of which standard ADRG sections a guide contains, this emits
  usability evidence: missing required sections fail, missing
  recommended sections warn. The same machinery covers the SDRG via
  `source_name`.
- Add
  [`evidence_sources()`](https://r4sub.github.io/r4subcore/reference/evidence_sources.md),
  a reference table of supported and planned evidence sources with their
  format, pillar, and entry point.
- Harden
  [`define_xml_to_evidence()`](https://r4sub.github.io/r4subcore/reference/define_xml_to_evidence.md)
  for real-world Define-XML variability. It now matches elements by
  local name, so it reads files regardless of how they mix the ODM and
  `def:` namespaces (a plain `//ItemGroupDef` query silently missed
  namespace-prefixed elements). Derivations are resolved from a
  referenced `MethodDef` when the `Origin` carries no inline text,
  missing optional elements are tolerated, and a partially parseable
  file now returns an informative warning.
- Clarified the package DESCRIPTION: “R4SUB” expands to “Ready for
  Submission” (previously “R for Regulatory Submission”, inconsistent
  with the rest of the ecosystem).

## r4subcore 0.1.2

- [`define_xml_to_evidence()`](https://r4sub.github.io/r4subcore/reference/define_xml_to_evidence.md):
  Parse Define-XML 2.0/2.1 files into evidence, emitting Q-DEFINE-001
  (dataset present), Q-DEFINE-002 (variable documented), and
  Q-DEFINE-003 (derivation present) indicators.
- [`export_evidence()`](https://r4sub.github.io/r4subcore/reference/export_evidence.md)
  /
  [`import_evidence()`](https://r4sub.github.io/r4subcore/reference/import_evidence.md):
  Save and reload validated evidence tables in CSV, RDS, or JSON format
  with metadata attributes.

## r4subcore 0.1.1

- Added ORCID for package author in `Authors@R`.
- Added `r4subdata` to `Suggests` - the companion example-data package
  is now on CRAN.

## r4subcore 0.1.0

CRAN release: 2026-02-20

- Initial CRAN release.
