# Read a Pinnacle 21 Report from Disk

Loads a Pinnacle 21 validation report (the export most teams already
produce for every submission) into a data.frame, ready for
[`p21_to_evidence()`](https://r4sub.github.io/r4subcore/reference/p21_to_evidence.md).
CSV exports are read with base R. Excel exports are read with the
`readxl` package when it is installed; if it is not, export the report
as CSV instead.

## Usage

``` r
read_p21_report(path, format = c("auto", "csv", "xlsx"), sheet = NULL)
```

## Arguments

- path:

  Path to the report file.

- format:

  One of `"auto"` (infer from the file extension), `"csv"`, or `"xlsx"`.
  Default `"auto"`.

- sheet:

  For Excel reports, the sheet to read. When `NULL` (the default), a
  sheet whose name looks like the findings list (matching "detail",
  "issue", or "finding") is used, otherwise the first sheet.

## Value

A data.frame of the report rows with the original column names.

## Details

This function only reads the file; it does not interpret the columns.
Pass the result to
[`p21_to_evidence()`](https://r4sub.github.io/r4subcore/reference/p21_to_evidence.md),
which detects the rule, message, severity, dataset, variable, and result
columns case-insensitively.

## See also

[`p21_to_evidence()`](https://r4sub.github.io/r4subcore/reference/p21_to_evidence.md)
to convert the result into evidence.

## Examples

``` r
# Simulate a small CSV export and read it back.
p21 <- data.frame(
  "Rule ID" = c("SD0001", "SD0002"),
  Message   = c("Missing variable label", "Invalid format"),
  Severity  = c("Error", "Warning"),
  Dataset   = c("ADSL", "ADAE"),
  Variable  = c("AGEU", "AESEQ"),
  Status    = c("Failed", "Warning"),
  check.names = FALSE
)
csv <- tempfile(fileext = ".csv")
utils::write.csv(p21, csv, row.names = FALSE)
report <- read_p21_report(csv)
#> ℹ Read 2 rows from /tmp/RtmpgpF09e/file1b097ba047d6.csv
report
#>   Rule ID                Message Severity Dataset Variable  Status
#> 1  SD0001 Missing variable label    Error    ADSL     AGEU  Failed
#> 2  SD0002         Invalid format  Warning    ADAE    AESEQ Warning
```
