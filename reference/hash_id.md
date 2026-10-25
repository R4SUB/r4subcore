# Generate a Stable Hash ID

Creates a deterministic hash from one or more character inputs, hashed
in memory with
[`rlang::hash()`](https://rlang.r-lib.org/reference/hash.html).

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
#> [1] "051a407df82eb34dac57a6a40d0368a0"
hash_id("my_study", "2024-01-01", prefix = "RUN")
#> [1] "RUN-ada9beca8a67f6c7118686582e9a1572"
```
