## Submission

This is an update of r4subcore from 0.1.0 (on CRAN) to 0.2.1. It is a feature
release for the R4SUB (Ready for Submission) ecosystem. Highlights:

* Evidence schema versioning with `evidence_schema_version()` and
  `migrate_evidence()`, so the shared evidence contract can evolve safely.
* `read_p21_report()` reads a Pinnacle 21 report from disk, and
  `p21_to_evidence()` / `adrg_to_evidence()` add new evidence sources.
* `define_xml_to_evidence()` hardened for namespace variability, plus
  `check_define_consistency()` comparing Define-XML to dataset contents.
* A large performance improvement to `hash_id()` (in-memory hashing).

See NEWS.md for the complete list.

## Test environments

* local: Windows 11 x64, R 4.5.x
* GitHub Actions: ubuntu-latest, windows-latest, macos-latest (R release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

r4subcore is imported by other R4SUB packages (r4subscore, r4subtrace,
r4subrisk, r4subprofile, r4subusability, r4subpharma) and the r4sub
meta-package. The changes in this release are additive; the existing evidence
schema and function interfaces are unchanged, so the reverse dependencies
already on CRAN continue to work. Updated reverse dependencies are being
submitted alongside this release.
