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
