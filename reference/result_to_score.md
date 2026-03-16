# Map Result to Numeric Score

Converts canonical result labels to numeric scores.

## Usage

``` r
result_to_score(result)
```

## Arguments

- result:

  Character vector of canonical result values (`pass`, `fail`, `warn`,
  `na`).

## Value

Numeric vector: pass=1, warn=0.5, fail=0, na=NA.

## Examples

``` r
result_to_score(c("pass", "fail", "warn", "na"))
#> [1] 1.0 0.0 0.5  NA
```
