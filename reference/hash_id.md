# Generate a Stable Hash ID

Creates a deterministic hash from one or more character inputs. Uses MD5
via base R's [digest-like
approach](https://rdrr.io/r/base/base-package.html) for a lightweight,
dependency-free implementation.

## Usage

``` r
hash_id(..., prefix = NULL)
```

## Arguments

- ...:

  Character values to hash together. Concatenated with `"|"`.

- prefix:

  Optional prefix prepended to the hash (e.g., `"RUN"`, `"IND"`).

## Value

A character string of the form `prefix-hexhash` or just `hexhash`.

## Examples

``` r
hash_id("ADSL", "rule_001")
#> [1] "a0570f2e91174ffbf899427461748fbd"
hash_id("my_study", "2024-01-01", prefix = "RUN")
#> [1] "RUN-b1f41f1eb58f223c4ede5384464281e5"
```
