# Standard ADRG Section Catalog

Returns the standard top-level and key subsections of the Analysis Data
Reviewer's Guide (ADRG), following the PHUSE ADRG completion guideline.
This is the checklist
[`adrg_to_evidence()`](https://r4sub.github.io/r4subcore/reference/adrg_to_evidence.md)
scores a guide against.

## Usage

``` r
adrg_sections()
```

## Value

A tibble with columns `section_id`, `section_name`, and `required`.

## Examples

``` r
adrg_sections()
#> # A tibble: 10 × 3
#>    section_id section_name                                         required
#>    <chr>      <chr>                                                <lgl>   
#>  1 1          Introduction                                         TRUE    
#>  2 2          Protocol Description                                 TRUE    
#>  3 3          Analysis Considerations Related to Multiple Datasets TRUE    
#>  4 4          Analysis Data Creation and Processing Overview       TRUE    
#>  5 5          Analysis Dataset Descriptions                        TRUE    
#>  6 6          Data Conformance Summary                             TRUE    
#>  7 1.3        Standards and Dictionary Inventory                   FALSE   
#>  8 3.4        Imputation and Derivation Methods                    FALSE   
#>  9 4.2        Analysis Dataset Dependencies                        FALSE   
#> 10 6.2        Conformance Issues Summary                           FALSE   
```
