# List the Variables in Each Dataset

Builds the `(dataset, variable)` table that
[`check_define_consistency()`](https://r4sub.github.io/r4subcore/reference/check_define_consistency.md)
compares against Define-XML. It reads only the names present in each
dataset; it does not open the file for any other purpose, and it emits
no scoring or evidence of its own. The companion on the Define-XML side
is
[`define_variables()`](https://r4sub.github.io/r4subcore/reference/define_variables.md).

## Usage

``` r
dataset_variables(x, dataset = NULL)
```

## Arguments

- x:

  A data frame, a named list of data frames, or a path to a directory of
  dataset files.

- dataset:

  For a single data frame, the dataset name to record. Ignored, and not
  required, for the list and directory forms.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
character columns `dataset` and `variable`, one row per variable in each
dataset.

## Details

The input can be:

- a single data frame, with its dataset name given through `dataset`;

- a named list of data frames, where each name is used as the dataset
  name;

- a path to a directory of files, one data frame each, where the file
  name (without extension) is used as the dataset name. `.rds` files are
  read with base R. SAS transport (`.xpt`) files are read when the
  optional `haven` package is installed; without it they are skipped.

## See also

[`check_define_consistency()`](https://r4sub.github.io/r4subcore/reference/check_define_consistency.md),
[`define_variables()`](https://r4sub.github.io/r4subcore/reference/define_variables.md).

## Examples

``` r
# A named list of data frames: the names become the dataset names.
dataset_variables(list(
  ADSL = data.frame(USUBJID = character(), AGE = numeric()),
  ADAE = data.frame(USUBJID = character(), AEDECOD = character())
))
#> # A tibble: 4 × 2
#>   dataset variable
#> * <chr>   <chr>   
#> 1 ADSL    USUBJID 
#> 2 ADSL    AGE     
#> 3 ADAE    USUBJID 
#> 4 ADAE    AEDECOD 

# A single data frame needs an explicit dataset name.
dataset_variables(data.frame(USUBJID = 1, SEX = "F"), dataset = "ADSL")
#> # A tibble: 2 × 2
#>   dataset variable
#>   <chr>   <chr>   
#> 1 ADSL    USUBJID 
#> 2 ADSL    SEX     

# A directory of .rds files, one data frame each.
dir <- tempfile("datasets")
dir.create(dir)
saveRDS(data.frame(USUBJID = 1, AGE = 40), file.path(dir, "ADSL.rds"))
saveRDS(data.frame(USUBJID = 1, AEDECOD = "X"), file.path(dir, "ADAE.rds"))
dataset_variables(dir)
#> # A tibble: 4 × 2
#>   dataset variable
#> * <chr>   <chr>   
#> 1 ADAE    USUBJID 
#> 2 ADAE    AEDECOD 
#> 3 ADSL    USUBJID 
#> 4 ADSL    AGE     

# Feed the result straight into check_define_consistency().
ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
define <- data.frame(dataset = "ADSL", variable = c("USUBJID", "AGE"))
data <- dataset_variables(list(ADSL = data.frame(USUBJID = 1, AGE = 40)))
suppressMessages(check_define_consistency(define, data, ctx))[, c("asset_id", "result")]
#>   asset_id result
#> 1     ADSL   pass
#> 2     ADSL   pass
```
