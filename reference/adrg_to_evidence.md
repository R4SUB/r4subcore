# Convert ADRG Section Coverage to Evidence

Scores an Analysis Data Reviewer's Guide for completeness against the
standard section catalog and emits R4SUB evidence rows, one per section.

## Usage

``` r
adrg_to_evidence(
  present,
  ctx,
  source_name = "adrg",
  source_version = NULL,
  standard = adrg_sections()
)
```

## Arguments

- present:

  A character vector of `section_id`s the guide contains, or a
  data.frame with a `section_id` column and a logical `present` column.

- ctx:

  An
  [r4sub_run_context](https://r4sub.github.io/r4subcore/reference/r4sub_run_context.md).

- source_name:

  Source label. Default `"adrg"`; use `"sdrg"` for a Study Data
  Reviewer's Guide.

- source_version:

  Optional source version string.

- standard:

  The section catalog to score against. Defaults to
  [`adrg_sections()`](https://r4sub.github.io/r4subcore/reference/adrg_sections.md).

## Value

An evidence data.frame, one row per standard section.

## Details

ADRG and SDRG documents are Word or PDF, and reliable text extraction
from them is out of scope for this package. `adrg_to_evidence()`
therefore takes a structured summary of which sections a guide contains,
which a document pipeline (or a human reviewer) can produce, and turns
it into evidence. A present section passes. A missing required section
fails at high severity; a missing recommended section warns at low
severity. The rows land in the usability pillar, since the reviewer's
guide is reviewer-facing documentation.

## Examples

``` r
ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
ev <- suppressMessages(adrg_to_evidence(c("1", "2", "3", "4", "5"), ctx))
ev[, c("indicator_id", "severity", "result")]
#>    indicator_id severity result
#> 1      U-ADRG-1     high   pass
#> 2      U-ADRG-2     high   pass
#> 3      U-ADRG-3     high   pass
#> 4      U-ADRG-4     high   pass
#> 5      U-ADRG-5     high   pass
#> 6      U-ADRG-6     high   fail
#> 7    U-ADRG-1_3      low   warn
#> 8    U-ADRG-3_4      low   warn
#> 9    U-ADRG-4_2      low   warn
#> 10   U-ADRG-6_2      low   warn
```
