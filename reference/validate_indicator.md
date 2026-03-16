# Validate Indicator Metadata

Checks that an indicator definition list is well-formed.

## Usage

``` r
validate_indicator(indicator)
```

## Arguments

- indicator:

  A list with required fields: `indicator_id`, `domain`, `description`.
  Optional fields: `expected_inputs`, `default_thresholds`, `tags`.

## Value

`TRUE` invisibly if valid; throws an error otherwise.

## Examples

``` r
validate_indicator(list(
  indicator_id = "P21-001",
  domain = "quality",
  description = "Missing required variable"
))
```
