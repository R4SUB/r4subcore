# Create a Run Context

A run context captures metadata for a particular evidence collection
run. It provides a unique `run_id`, study identifier, environment label,
and timestamps used throughout evidence ingestion.

## Usage

``` r
r4sub_run_context(
  study_id,
  environment = c("DEV", "UAT", "PROD"),
  user = NULL,
  run_id = NULL,
  timestamp = Sys.time()
)
```

## Arguments

- study_id:

  Character. Study identifier (e.g., `"ABC123"`).

- environment:

  Character. One of `"DEV"`, `"UAT"`, `"PROD"`.

- user:

  Character or `NULL`. Username; defaults to system user.

- run_id:

  Character or `NULL`. If `NULL`, a unique ID is generated.

- timestamp:

  POSIXct. Defaults to current time.

## Value

A list of class `r4sub_run_context` with elements: `run_id`, `study_id`,
`environment`, `user`, `created_at`.

## Examples

``` r
ctx <- r4sub_run_context(study_id = "STUDY001", environment = "DEV")
#> ℹ Run context created: "R4S-20260316112308-qm66mr5e"
ctx$run_id
#> [1] "R4S-20260316112308-qm66mr5e"
ctx$study_id
#> [1] "STUDY001"
```
