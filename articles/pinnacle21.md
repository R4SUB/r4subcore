# Reading Pinnacle 21 reports into evidence

Most clinical programming teams already run Pinnacle 21 on every
submission and have the report on disk. That report is the fastest way
to get real evidence into R4SUB: there is no table to build by hand.
This vignette shows the two steps, reading the report and converting it
to the shared evidence table.

``` r

library(r4subcore)
```

## A Pinnacle 21 export

A Pinnacle 21 report lists one row per finding: a rule identifier, a
message, a severity, the dataset and variable it concerns, and a status.
Here we write a small export to a temporary CSV so the vignette is
self-contained. Your real CSV or Excel export has the same shape.

``` r

p21 <- data.frame(
  "Rule ID" = c("SD0001", "SD0002", "AD0018", "SD1077"),
  Message   = c("Variable label is missing",
                "Value not in controlled terminology",
                "Expected variable is present",
                "Inconsistent value length"),
  Severity  = c("Error", "Warning", "Notice", "Error"),
  Dataset   = c("ADSL", "ADAE", "ADSL", "ADLB"),
  Variable  = c("AGEU", "AESEV", "RACE", "LBORRES"),
  Status    = c("Failed", "Warning", "Passed", "Failed"),
  check.names = FALSE
)

csv <- tempfile(fileext = ".csv")
utils::write.csv(p21, csv, row.names = FALSE)
```

## Reading the report

[`read_p21_report()`](https://r4sub.github.io/r4subcore/reference/read_p21_report.md)
loads the file. It reads CSV with base R and Excel through the `readxl`
package when that is installed. It only reads the file; it does not
interpret the columns yet.

``` r

report <- read_p21_report(csv)
#> ℹ Read 4 rows from /tmp/RtmpG6a06a/file1dd5540150c6.csv
report
#>   Rule ID                             Message Severity Dataset Variable  Status
#> 1  SD0001           Variable label is missing    Error    ADSL     AGEU  Failed
#> 2  SD0002 Value not in controlled terminology  Warning    ADAE    AESEV Warning
#> 3  AD0018        Expected variable is present   Notice    ADSL     RACE  Passed
#> 4  SD1077           Inconsistent value length    Error    ADLB  LBORRES  Failed
```

## Converting to evidence

[`p21_to_evidence()`](https://r4sub.github.io/r4subcore/reference/p21_to_evidence.md)
maps the report onto the evidence schema shared across the ecosystem. It
detects the rule, message, severity, dataset, variable, and result
columns case-insensitively, so it tolerates the small naming differences
between Pinnacle 21 versions. Pinnacle 21 severities map onto the
evidence vocabulary: `Error` becomes `high`, `Warning` becomes `medium`,
and `Notice` becomes `info`.

``` r

ctx <- r4sub_run_context("STUDY1", "DEV")
#> ℹ Run context created: "R4S-20261025204227-wl4dieex"
ev  <- p21_to_evidence(report, ctx)
#> ℹ Parsed 4 rows from Pinnacle21 output
#> ✔ Evidence table created: 4 rows

ev[, c("indicator_id", "severity", "result", "location")]
#>   indicator_id severity result     location
#> 1       SD0001     high   fail    ADSL:AGEU
#> 2       SD0002   medium   warn   ADAE:AESEV
#> 3       AD0018     info   pass    ADSL:RACE
#> 4       SD1077     high   fail ADLB:LBORRES
```

The result is an ordinary evidence table. It validates against the
schema, so it scores like any other evidence, and it can be combined
with evidence from other sources (Define-XML, the ADRG, traceability)
with
[`bind_evidence()`](https://r4sub.github.io/r4subcore/reference/bind_evidence.md)
before computing a Submission Confidence Index.

``` r

validate_evidence(ev)
```
