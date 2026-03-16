# Canonical Result Values

Maps common result/status labels to the canonical set: `pass`, `fail`,
`warn`, `na`.

## Usage

``` r
canon_result(x)
```

## Arguments

- x:

  Character vector of result values.

## Value

Character vector with canonical result labels.

## Examples

``` r
canon_result(c("PASS", "Failed", "Warning", "N/A"))
#> [1] "pass" "fail" "warn" "na"  
```
