# Normalize to 0–1 Range

Applies min-max normalization to a numeric vector, optionally clamping
values to \[0, 1\].

## Usage

``` r
normalize_01(x, direction = c("higher_better", "lower_better"), clamp = TRUE)
```

## Arguments

- x:

  Numeric vector.

- direction:

  Character. `"higher_better"` (default) maps max to 1; `"lower_better"`
  maps min to 1.

- clamp:

  Logical. If `TRUE`, clamp output to \[0, 1\].

## Value

Numeric vector normalized to 0–1.

## Examples

``` r
normalize_01(c(10, 20, 30, 40, 50))
#> [1] 0.00 0.25 0.50 0.75 1.00
normalize_01(c(10, 20, 30), direction = "lower_better")
#> [1] 1.0 0.5 0.0
```
