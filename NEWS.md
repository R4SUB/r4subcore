# r4subcore (development version)

- The evidence table now carries a schema version. `as_evidence()` stamps it,
  `evidence_schema_version()` reports the current version or the version of a
  given table, and `migrate_evidence()` upgrades a table written by an older
  release. `validate_evidence()` tolerates an unstamped table but refuses an
  unknown or newer version, so a table written by a future release cannot be
  silently mis-scored.
- Fix `import_evidence()`: a CSV or JSON round-trip could leave an all-`NA`
  character column (for example `source_version`) typed as logical, which then
  failed validation. Every schema column is now coerced back to its declared
  type on import.
- Add `evidence_schema_spec()`, which publishes the evidence schema as a tidy
  table (column, type, required, allowed values, description) for use as a data
  dictionary or reference.
- Add vignette: "The R4SUB Evidence Schema", the formal specification of the
  evidence table shared across the ecosystem.
- Add `adrg_to_evidence()` and `adrg_sections()` for Analysis Data Reviewer's
  Guide coverage. Given a structured summary of which standard ADRG sections a
  guide contains, this emits usability evidence: missing required sections fail,
  missing recommended sections warn. The same machinery covers the SDRG via
  `source_name`.
- Add `evidence_sources()`, a reference table of supported and planned evidence
  sources with their format, pillar, and entry point.
- Harden `define_xml_to_evidence()` for real-world Define-XML variability. It now
  matches elements by local name, so it reads files regardless of how they mix
  the ODM and `def:` namespaces (a plain `//ItemGroupDef` query silently missed
  namespace-prefixed elements). Derivations are resolved from a referenced
  `MethodDef` when the `Origin` carries no inline text, missing optional
  elements are tolerated, and a partially parseable file now returns an
  informative warning.
- Clarified the package DESCRIPTION: "R4SUB" expands to "Ready for Submission"
  (previously "R for Regulatory Submission", inconsistent with the rest of the
  ecosystem).

# r4subcore 0.1.2

- `define_xml_to_evidence()`: Parse Define-XML 2.0/2.1 files into evidence,
  emitting Q-DEFINE-001 (dataset present), Q-DEFINE-002 (variable documented),
  and Q-DEFINE-003 (derivation present) indicators.
- `export_evidence()` / `import_evidence()`: Save and reload validated evidence
  tables in CSV, RDS, or JSON format with metadata attributes.

# r4subcore 0.1.1

- Added ORCID for package author in `Authors@R`.
- Added `r4subdata` to `Suggests` — the companion example-data package is now
  on CRAN.

# r4subcore 0.1.0

- Initial CRAN release.
