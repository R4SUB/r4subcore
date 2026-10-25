# Extract the Variables Declared in a Define-XML File

Returns one row per variable declared in Define-XML, following the
`ItemGroupDef` to `ItemRef` to `ItemDef` structure: the dataset name
comes from the `ItemGroupDef`, and the variable name from the `ItemDef`
its `ItemRef` points at. Elements are matched by local name, so the
result does not depend on how the file mixes the ODM and `def:`
namespaces.

## Usage

``` r
define_variables(file)
```

## Arguments

- file:

  Path to a Define-XML file.

## Value

A data.frame with columns `dataset` and `variable`.

## See also

[`check_define_consistency()`](https://r4sub.github.io/r4subcore/reference/check_define_consistency.md).
