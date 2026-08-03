#' Standard ADRG Section Catalog
#'
#' Returns the standard top-level and key subsections of the Analysis Data
#' Reviewer's Guide (ADRG), following the PHUSE ADRG completion guideline. This
#' is the checklist [adrg_to_evidence()] scores a guide against.
#'
#' @return A tibble with columns `section_id`, `section_name`, and `required`.
#'
#' @examples
#' adrg_sections()
#'
#' @export
adrg_sections <- function() {
  tibble::tibble(
    section_id = c("1", "2", "3", "4", "5", "6",
                   "1.3", "3.4", "4.2", "6.2"),
    section_name = c(
      "Introduction",
      "Protocol Description",
      "Analysis Considerations Related to Multiple Datasets",
      "Analysis Data Creation and Processing Overview",
      "Analysis Dataset Descriptions",
      "Data Conformance Summary",
      "Standards and Dictionary Inventory",
      "Imputation and Derivation Methods",
      "Analysis Dataset Dependencies",
      "Conformance Issues Summary"
    ),
    required = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                 FALSE, FALSE, FALSE, FALSE)
  )
}


#' Convert ADRG Section Coverage to Evidence
#'
#' Scores an Analysis Data Reviewer's Guide for completeness against the
#' standard section catalog and emits R4SUB evidence rows, one per section.
#'
#' @details
#' ADRG and SDRG documents are Word or PDF, and reliable text extraction from
#' them is out of scope for this package. `adrg_to_evidence()` therefore takes a
#' structured summary of which sections a guide contains, which a document
#' pipeline (or a human reviewer) can produce, and turns it into evidence. A
#' present section passes. A missing required section fails at high severity; a
#' missing recommended section warns at low severity. The rows land in the
#' usability pillar, since the reviewer's guide is reviewer-facing documentation.
#'
#' @param present A character vector of `section_id`s the guide contains, or a
#'   data.frame with a `section_id` column and a logical `present` column.
#' @param ctx An [r4sub_run_context].
#' @param source_name Source label. Default `"adrg"`; use `"sdrg"` for a Study
#'   Data Reviewer's Guide.
#' @param source_version Optional source version string.
#' @param standard The section catalog to score against. Defaults to
#'   [adrg_sections()].
#'
#' @return An evidence data.frame, one row per standard section.
#'
#' @examples
#' ctx <- suppressMessages(r4sub_run_context("STUDY1", "DEV"))
#' ev <- suppressMessages(adrg_to_evidence(c("1", "2", "3", "4", "5"), ctx))
#' ev[, c("indicator_id", "severity", "result")]
#'
#' @export
adrg_to_evidence <- function(present,
                             ctx,
                             source_name = "adrg",
                             source_version = NULL,
                             standard = adrg_sections()) {
  stopifnot(inherits(ctx, "r4sub_run_context"))

  present_ids <- normalize_present(present)

  unknown <- setdiff(present_ids, standard$section_id)
  if (length(unknown) > 0L) {
    cli::cli_warn(
      "Ignoring unknown ADRG section id{?s}: {.val {unknown}}."
    )
  }

  is_present <- standard$section_id %in% present_ids
  n <- nrow(standard)

  severity <- ifelse(standard$required, "high", "low")
  result   <- ifelse(is_present, "pass",
                     ifelse(standard$required, "fail", "warn"))
  message  <- ifelse(
    is_present,
    paste0("Section ", standard$section_id, " (", standard$section_name,
           ") present"),
    paste0("Section ", standard$section_id, " (", standard$section_name,
           ") missing")
  )

  ev_df <- data.frame(
    run_id           = ctx$run_id,
    study_id         = ctx$study_id,
    asset_type       = "other",
    asset_id         = toupper(source_name),
    source_name      = source_name,
    source_version   = if (!is.null(source_version)) source_version else NA_character_,
    indicator_id     = paste0("U-ADRG-", gsub("\\.", "_", standard$section_id)),
    indicator_name   = paste0("ADRG section: ", standard$section_name),
    indicator_domain = "usability",
    severity         = severity,
    result           = result,
    metric_value     = as.numeric(is_present),
    metric_unit      = "present",
    message          = message,
    location         = paste0("ADRG:", standard$section_id),
    evidence_payload = "{}",
    created_at       = Sys.time(),
    stringsAsFactors = FALSE
  )

  cli::cli_alert_info(
    "Scored {n} ADRG section{?s}: {sum(is_present)} present, {sum(!is_present)} missing"
  )
  as_evidence(ev_df, ctx = ctx)
}


#' Supported and Planned Evidence Sources
#'
#' A reference table of the evidence sources R4SUB can ingest, their status, the
#' format they come from, and the pillar they feed. Use it to see at a glance
#' what is wired up today and what is on the roadmap.
#'
#' @return A tibble with columns `source`, `status`, `format`, `pillar`, and
#'   `entry_point`.
#'
#' @examples
#' evidence_sources()
#'
#' @export
evidence_sources <- function() {
  tibble::tibble(
    source = c("Pinnacle 21", "Define-XML 2.0/2.1", "ADRG / SDRG",
               "ADaM/SDTM metadata", "Custom checks"),
    status = c("supported", "supported", "supported",
               "supported", "supported"),
    format = c("CSV export", "XML", "structured section summary",
               "data.frame", "any"),
    pillar = c("quality", "quality", "usability", "trace", "any"),
    entry_point = c("p21_to_evidence()", "define_xml_to_evidence()",
                    "adrg_to_evidence()", "r4subtrace, r4subusability",
                    "as_evidence()")
  )
}


# Accept either a character vector of section ids or a data.frame with
# section_id and present columns; return the present ids.
normalize_present <- function(present) {
  if (is.character(present)) {
    return(unique(stats::na.omit(present)))
  }
  if (is.data.frame(present)) {
    if (!all(c("section_id", "present") %in% names(present))) {
      cli::cli_abort(
        "A data.frame {.arg present} needs {.val section_id} and \\
         {.val present} columns."
      )
    }
    return(as.character(present$section_id[isTRUE_vec(present$present)]))
  }
  cli::cli_abort(
    "{.arg present} must be a character vector or a data.frame."
  )
}

isTRUE_vec <- function(x) !is.na(x) & x
