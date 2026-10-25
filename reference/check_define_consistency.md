# Check Define-XML Against Dataset Contents

Compares the variables declared in Define-XML against the variables
actually present in the datasets, and emits evidence for each: a match,
a variable documented in Define but absent from the data, or a variable
present in the data but not documented. Drift between Define-XML and the
data is a common submission problem that reviewers notice quickly, and
this makes it a scored indicator. The comparison is case-insensitive on
dataset and variable names.

## Usage

``` r
check_define_consistency(
  define,
  data,
  ctx,
  source_name = "define_vs_data",
  source_version = NULL
)
```

## Arguments

- define:

  Either a path to a Define-XML file, or a data.frame with `dataset` and
  `variable` columns (as returned by
  [`define_variables()`](https://r4sub.github.io/r4subcore/reference/define_variables.md)).

- data:

  A data.frame with `dataset` and `variable` columns listing the
  variables actually present in each dataset.

- ctx:

  A
  [r4sub_run_context](https://r4sub.github.io/r4subcore/reference/r4sub_run_context.md).

- source_name, source_version:

  Provenance recorded on the evidence.

## Value

A data.frame conforming to the evidence schema, one row per variable
checked, under indicator `Q-DEFINE-CONSIST`.

## See also

[`define_variables()`](https://r4sub.github.io/r4subcore/reference/define_variables.md),
[`define_xml_to_evidence()`](https://r4sub.github.io/r4subcore/reference/define_xml_to_evidence.md).

## Examples

``` r
ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
define <- data.frame(
  dataset  = c("ADSL", "ADSL", "ADSL"),
  variable = c("USUBJID", "AGE", "SEX")
)
data <- data.frame(
  dataset  = c("ADSL", "ADSL", "ADSL"),
  variable = c("USUBJID", "AGE", "RACE")  # SEX missing, RACE undocumented
)
ev <- suppressMessages(check_define_consistency(define, data, ctx))
ev[, c("asset_id", "result", "message")]
#>   asset_id result
#> 1     ADSL   pass
#> 2     ADSL   pass
#> 3     ADSL   fail
#> 4     ADSL   fail
#>                                                             message
#> 1                            ADSL.USUBJID is documented and present
#> 2                                ADSL.AGE is documented and present
#> 3     ADSL.SEX is documented in Define-XML but absent from the data
#> 4 ADSL.RACE is present in the data but not documented in Define-XML
```
