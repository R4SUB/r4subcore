# Canonical Severity Values

Maps common severity labels (case-insensitive) to the canonical set.

## Usage

``` r
canon_severity(x)
```

## Arguments

- x:

  Character vector of severity values.

## Value

Character vector with canonical severity labels.

## Examples

``` r
canon_severity(c("HIGH", "Low", "warning", "Error"))
#> [1] "high"   "low"    "medium" "high"  
```
