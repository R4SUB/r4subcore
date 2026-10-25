# Supported and Planned Evidence Sources

A reference table of the evidence sources R4SUB can ingest, their
status, the format they come from, and the pillar they feed. Use it to
see at a glance what is wired up today and what is on the roadmap.

## Usage

``` r
evidence_sources()
```

## Value

A tibble with columns `source`, `status`, `format`, `pillar`, and
`entry_point`.

## Examples

``` r
evidence_sources()
#> # A tibble: 5 × 5
#>   source             status    format                     pillar    entry_point 
#>   <chr>              <chr>     <chr>                      <chr>     <chr>       
#> 1 Pinnacle 21        supported CSV export                 quality   p21_to_evid…
#> 2 Define-XML 2.0/2.1 supported XML                        quality   define_xml_…
#> 3 ADRG / SDRG        supported structured section summary usability adrg_to_evi…
#> 4 ADaM/SDTM metadata supported data.frame                 trace     r4subtrace,…
#> 5 Custom checks      supported any                        any       as_evidence…
```
