# Register an Indicator

Adds an indicator definition to the local in-memory registry.

## Usage

``` r
register_indicator(
  indicator_id,
  domain,
  description,
  expected_inputs = character(0),
  default_thresholds = numeric(0),
  tags = character(0)
)
```

## Arguments

- indicator_id:

  Character. Stable identifier for the indicator.

- domain:

  Character. One of `"quality"`, `"trace"`, `"risk"`, `"usability"`.

- description:

  Character. Human-readable description.

- expected_inputs:

  Character vector. Evidence source types this indicator expects.

- default_thresholds:

  Named numeric vector. Optional thresholds.

- tags:

  Character vector. Optional tags (e.g., `"define"`, `"adam"`).

## Value

The indicator definition list, invisibly.

## Examples

``` r
register_indicator(
  indicator_id = "P21-001",
  domain = "quality",
  description = "Required variable is missing from dataset"
)
#> ✔ Registered indicator: "P21-001"
```
