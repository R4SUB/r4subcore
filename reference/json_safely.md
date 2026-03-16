# Safely Serialize to JSON String

Converts an R object to a valid JSON string. Returns `"{}"` on failure
or for `NULL`/empty inputs.

## Usage

``` r
json_safely(x)
```

## Arguments

- x:

  An R object to serialize.

## Value

A single character string containing valid JSON.

## Examples

``` r
json_safely(list(a = 1, b = "hello"))
#> [1] "{\"a\":1,\"b\":\"hello\"}"
json_safely(NULL)
#> [1] "{}"
```
