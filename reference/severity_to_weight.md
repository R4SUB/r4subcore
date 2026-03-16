# Map Severity to Numeric Weight

Converts canonical severity labels to numeric penalty multipliers on a
0–1 scale.

## Usage

``` r
severity_to_weight(severity)
```

## Arguments

- severity:

  Character vector of canonical severity values (`info`, `low`,
  `medium`, `high`, `critical`).

## Value

Numeric vector of weights.

## Details

Default mapping:

- `info` = 0.00

- `low` = 0.25

- `medium` = 0.50

- `high` = 0.75

- `critical` = 1.00

## Examples

``` r
severity_to_weight(c("low", "high", "critical"))
#> [1] 0.25 0.75 1.00
```
