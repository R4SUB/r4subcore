# r4subcore (development version)

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
