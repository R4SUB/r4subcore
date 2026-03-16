# Evidence Table Schema Definition

Returns the column specification for the R4SUB evidence table. Each
element describes a column's expected R type and, where applicable, the
set of allowed values.

## Usage

``` r
evidence_schema()
```

## Value

A named list. Each element is a list with `type` (character) and
optionally `allowed` (character vector) or `nullable` (logical).

## Examples

``` r
str(evidence_schema())
#> List of 17
#>  $ run_id          :List of 2
#>   ..$ type    : chr "character"
#>   ..$ nullable: logi FALSE
#>  $ study_id        :List of 2
#>   ..$ type    : chr "character"
#>   ..$ nullable: logi FALSE
#>  $ asset_type      :List of 3
#>   ..$ type    : chr "character"
#>   ..$ nullable: logi FALSE
#>   ..$ allowed : chr [1:6] "dataset" "define" "program" "validation" ...
#>  $ asset_id        :List of 2
#>   ..$ type    : chr "character"
#>   ..$ nullable: logi FALSE
#>  $ source_name     :List of 2
#>   ..$ type    : chr "character"
#>   ..$ nullable: logi FALSE
#>  $ source_version  :List of 2
#>   ..$ type    : chr "character"
#>   ..$ nullable: logi TRUE
#>  $ indicator_id    :List of 2
#>   ..$ type    : chr "character"
#>   ..$ nullable: logi FALSE
#>  $ indicator_name  :List of 2
#>   ..$ type    : chr "character"
#>   ..$ nullable: logi FALSE
#>  $ indicator_domain:List of 3
#>   ..$ type    : chr "character"
#>   ..$ nullable: logi FALSE
#>   ..$ allowed : chr [1:4] "quality" "trace" "risk" "usability"
#>  $ severity        :List of 3
#>   ..$ type    : chr "character"
#>   ..$ nullable: logi FALSE
#>   ..$ allowed : chr [1:5] "info" "low" "medium" "high" ...
#>  $ result          :List of 3
#>   ..$ type    : chr "character"
#>   ..$ nullable: logi FALSE
#>   ..$ allowed : chr [1:4] "pass" "fail" "warn" "na"
#>  $ metric_value    :List of 2
#>   ..$ type    : chr "double"
#>   ..$ nullable: logi TRUE
#>  $ metric_unit     :List of 2
#>   ..$ type    : chr "character"
#>   ..$ nullable: logi TRUE
#>  $ message         :List of 2
#>   ..$ type    : chr "character"
#>   ..$ nullable: logi TRUE
#>  $ location        :List of 2
#>   ..$ type    : chr "character"
#>   ..$ nullable: logi TRUE
#>  $ evidence_payload:List of 2
#>   ..$ type    : chr "character"
#>   ..$ nullable: logi TRUE
#>  $ created_at      :List of 2
#>   ..$ type    : chr "POSIXct"
#>   ..$ nullable: logi FALSE
```
