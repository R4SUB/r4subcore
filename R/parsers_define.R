#' Parse Define-XML to Evidence
#'
#' Reads a Define-XML 2.0/2.1 file and extracts dataset, variable, and
#' derivation completeness checks into the standard evidence table format.
#' Three indicators are evaluated for each asset:
#' \describe{
#'   \item{Q-DEFINE-001}{Dataset is present and has a non-empty label.}
#'   \item{Q-DEFINE-002}{Variable is documented (has label and dataType).}
#'   \item{Q-DEFINE-003}{Derivation text is present for derived variables.}
#' }
#'
#' @param file Character. Path to a Define-XML file (`.xml`).
#' @param ctx An [r4sub_run_context] providing run and study metadata.
#' @param source_version Character. Version label for the Define-XML standard.
#'   Default: `"2.1"`.
#'
#' @return A data.frame conforming to the evidence schema, one row per
#'   dataset-level check (Q-DEFINE-001), per variable check (Q-DEFINE-002),
#'   and per derivation check (Q-DEFINE-003).
#'
#' @examples
#' \donttest{
#' # Build a minimal Define-XML 2.1 document
#' xml_txt <- '<?xml version="1.0" encoding="UTF-8"?>
#' <ODM xmlns="http://www.cdisc.org/ns/odm/v1.3"
#'      xmlns:def="http://www.cdisc.org/ns/def/v2.1">
#'   <Study OID="STUDY001">
#'     <MetaDataVersion OID="MDV.001" Name="Define-XML 2.1">
#'       <def:ItemGroupDef OID="IG.ADSL" Name="ADSL" SASDatasetName="ADSL"
#'                         Repeating="No" Purpose="Analysis"
#'                         def:Label="Subject-Level Analysis Dataset">
#'         <ItemRef ItemOID="IT.ADSL.USUBJID" Mandatory="Yes"/>
#'         <ItemRef ItemOID="IT.ADSL.AGE" Mandatory="Yes"/>
#'       </def:ItemGroupDef>
#'       <ItemDef OID="IT.ADSL.USUBJID" Name="USUBJID"
#'                DataType="text" Length="20"
#'                SASFieldName="USUBJID">
#'         <Description><TranslatedText>Unique Subject Identifier</TranslatedText></Description>
#'       </ItemDef>
#'       <ItemDef OID="IT.ADSL.AGE" Name="AGE"
#'                DataType="integer" Length="8"
#'                SASFieldName="AGE">
#'         <Description><TranslatedText>Age</TranslatedText></Description>
#'         <def:Origin Type="Derived">
#'           <def:Description><TranslatedText>Derived from RFSTDTC</TranslatedText></def:Description>
#'         </def:Origin>
#'       </ItemDef>
#'     </MetaDataVersion>
#'   </Study>
#' </ODM>'
#' tmp <- tempfile(fileext = ".xml")
#' writeLines(xml_txt, tmp)
#' ctx <- r4sub_run_context("STUDY001", "DEV")
#' ev <- define_xml_to_evidence(tmp, ctx)
#' nrow(ev)
#' }
#'
#' @importFrom xml2 read_xml xml_find_all xml_attr xml_text xml_ns_strip
#' @importFrom cli cli_alert_info cli_alert_warning
#' @export
define_xml_to_evidence <- function(file, ctx, source_version = "2.1") {
  stopifnot(
    is.character(file), length(file) == 1L, file.exists(file),
    inherits(ctx, "r4sub_run_context"),
    is.character(source_version), length(source_version) == 1L
  )

  doc <- xml2::read_xml(file)
  # Strip all namespaces for simpler xpath queries
  xml2::xml_ns_strip(doc)

  # ---- 1. Extract ItemGroupDef (datasets) ----
  item_groups <- xml2::xml_find_all(doc, "//ItemGroupDef")
  n_groups <- length(item_groups)
  cli::cli_alert_info("Define-XML: found {n_groups} dataset(s) (ItemGroupDef)")

  # ---- 2. Extract ItemDef (variables) ----
  item_defs <- xml2::xml_find_all(doc, "//ItemDef")
  n_items <- length(item_defs)
  cli::cli_alert_info("Define-XML: found {n_items} variable definition(s) (ItemDef)")

  rows <- list()

  # ---- Q-DEFINE-001: dataset present and labelled ----
  for (ig in item_groups) {
    ds_name  <- xml2::xml_attr(ig, "Name")
    ds_label <- xml2::xml_attr(ig, "Label")
    # Also accept def:Label attribute (common in Define-XML 2.0)
    if (is.na(ds_label) || !nzchar(ds_label)) {
      ds_label <- xml2::xml_attr(ig, "def:Label")
    }
    # Fall back: look for a child Label element
    if (is.na(ds_label) || !nzchar(ds_label)) {
      label_node <- xml2::xml_find_all(ig, ".//Description/TranslatedText")
      if (length(label_node) > 0) {
        ds_label <- xml2::xml_text(label_node[[1]])
      }
    }

    has_label  <- !is.na(ds_label) && nzchar(ds_label)
    result_val <- if (has_label) "pass" else "fail"
    sev_val    <- if (has_label) "info" else "high"
    msg_val    <- if (has_label) {
      sprintf("Dataset %s is present with label: %s", ds_name, ds_label)
    } else {
      sprintf("Dataset %s is missing a label in Define-XML", ds_name)
    }
    metric_val <- if (has_label) 1.0 else 0.0

    payload <- sprintf(
      '{"dataset":"%s","label":"%s","has_label":%s}',
      ds_name,
      if (!is.na(ds_label)) ds_label else "",
      tolower(as.character(has_label))
    )

    rows[[length(rows) + 1L]] <- list(
      asset_type       = "define",
      asset_id         = if (!is.na(ds_name)) ds_name else "define.xml",
      source_name      = "define_xml",
      source_version   = source_version,
      indicator_id     = "Q-DEFINE-001",
      indicator_name   = "Dataset Present in Define-XML",
      indicator_domain = "quality",
      severity         = sev_val,
      result           = result_val,
      metric_value     = metric_val,
      metric_unit      = "score",
      message          = msg_val,
      location         = if (!is.na(ds_name)) ds_name else "define.xml",
      evidence_payload = payload
    )
  }

  # If no datasets found at all, emit a single fail row
  if (n_groups == 0L) {
    cli::cli_alert_warning("No ItemGroupDef elements found in Define-XML")
    rows[[length(rows) + 1L]] <- list(
      asset_type       = "define",
      asset_id         = "define.xml",
      source_name      = "define_xml",
      source_version   = source_version,
      indicator_id     = "Q-DEFINE-001",
      indicator_name   = "Dataset Present in Define-XML",
      indicator_domain = "quality",
      severity         = "critical",
      result           = "fail",
      metric_value     = 0.0,
      metric_unit      = "score",
      message          = "No ItemGroupDef elements found; Define-XML may be empty or malformed",
      location         = "define.xml",
      evidence_payload = '{"dataset":null,"has_label":false}'
    )
  }

  # ---- Q-DEFINE-002: variable documented (has label + dataType) ----
  # Build a lookup: ItemOID -> parent dataset name via ItemRef
  # Map from OID to dataset
  oid_to_ds <- list()
  for (ig in item_groups) {
    ds_name  <- xml2::xml_attr(ig, "Name")
    item_refs <- xml2::xml_find_all(ig, ".//ItemRef")
    for (ir in item_refs) {
      item_oid <- xml2::xml_attr(ir, "ItemOID")
      if (!is.na(item_oid)) {
        oid_to_ds[[item_oid]] <- if (!is.na(ds_name)) ds_name else "define.xml"
      }
    }
  }

  for (id_node in item_defs) {
    oid       <- xml2::xml_attr(id_node, "OID")
    var_name  <- xml2::xml_attr(id_node, "Name")
    data_type <- xml2::xml_attr(id_node, "DataType")

    # Label from Description/TranslatedText child
    label_nodes <- xml2::xml_find_all(id_node, ".//Description/TranslatedText")
    var_label <- if (length(label_nodes) > 0) xml2::xml_text(label_nodes[[1]]) else NA_character_

    ds_name <- if (!is.null(oid) && !is.na(oid) && !is.null(oid_to_ds[[oid]])) {
      oid_to_ds[[oid]]
    } else {
      "define.xml"
    }

    has_label    <- !is.na(var_label)    && nzchar(var_label)
    has_datatype <- !is.na(data_type)    && nzchar(data_type)
    documented   <- has_label && has_datatype

    result_val <- if (documented) "pass" else if (has_label || has_datatype) "warn" else "fail"
    sev_val    <- if (documented) "info" else if (has_label || has_datatype) "medium" else "high"
    metric_val <- as.double(has_label) * 0.5 + as.double(has_datatype) * 0.5
    msg_val    <- if (documented) {
      sprintf("Variable %s in %s is fully documented (label + dataType)", var_name, ds_name)
    } else if (!has_label && !has_datatype) {
      sprintf("Variable %s in %s is missing both label and dataType", var_name, ds_name)
    } else if (!has_label) {
      sprintf("Variable %s in %s is missing label", var_name, ds_name)
    } else {
      sprintf("Variable %s in %s is missing dataType", var_name, ds_name)
    }

    payload <- sprintf(
      '{"variable":"%s","dataset":"%s","dataType":"%s","has_label":%s}',
      var_name,
      ds_name,
      if (!is.na(data_type)) data_type else "",
      tolower(as.character(has_label))
    )

    rows[[length(rows) + 1L]] <- list(
      asset_type       = "define",
      asset_id         = ds_name,
      source_name      = "define_xml",
      source_version   = source_version,
      indicator_id     = "Q-DEFINE-002",
      indicator_name   = "Variable Documented in Define-XML",
      indicator_domain = "quality",
      severity         = sev_val,
      result           = result_val,
      metric_value     = metric_val,
      metric_unit      = "score",
      message          = msg_val,
      location         = paste0(ds_name, ":", var_name),
      evidence_payload = payload
    )
  }

  # ---- Q-DEFINE-003: derivation text present for derived variables ----
  for (id_node in item_defs) {
    oid      <- xml2::xml_attr(id_node, "OID")
    var_name <- xml2::xml_attr(id_node, "Name")
    ds_name  <- if (!is.null(oid) && !is.na(oid) && !is.null(oid_to_ds[[oid]])) {
      oid_to_ds[[oid]]
    } else {
      "define.xml"
    }

    # Look for Origin elements
    origin_nodes <- xml2::xml_find_all(id_node, ".//Origin")
    if (length(origin_nodes) == 0L) next  # not derived, skip

    for (orig_node in origin_nodes) {
      origin_type <- xml2::xml_attr(orig_node, "Type")
      is_derived  <- !is.na(origin_type) &&
        tolower(origin_type) %in% c("derived", "assigned", "predecessor", "algorithm")

      if (!is_derived) next

      # Look for derivation description text
      deriv_nodes <- xml2::xml_find_all(
        orig_node,
        ".//Description/TranslatedText"
      )
      deriv_text <- if (length(deriv_nodes) > 0) {
        xml2::xml_text(deriv_nodes[[1]])
      } else {
        NA_character_
      }

      has_deriv  <- !is.na(deriv_text) && nzchar(deriv_text)
      result_val <- if (has_deriv) "pass" else "fail"
      sev_val    <- if (has_deriv) "info" else "high"
      metric_val <- if (has_deriv) 1.0 else 0.0
      msg_val    <- if (has_deriv) {
        sprintf("Derivation text present for %s in %s: %s",
                var_name, ds_name, substr(deriv_text, 1L, 80L))
      } else {
        sprintf("Derived variable %s in %s is missing derivation text (Origin Type=%s)",
                var_name, ds_name, origin_type)
      }

      payload <- sprintf(
        '{"variable":"%s","dataset":"%s","origin_type":"%s","has_derivation":%s}',
        var_name, ds_name,
        if (!is.na(origin_type)) origin_type else "",
        tolower(as.character(has_deriv))
      )

      rows[[length(rows) + 1L]] <- list(
        asset_type       = "define",
        asset_id         = ds_name,
        source_name      = "define_xml",
        source_version   = source_version,
        indicator_id     = "Q-DEFINE-003",
        indicator_name   = "Derivation Present in Define-XML",
        indicator_domain = "quality",
        severity         = sev_val,
        result           = result_val,
        metric_value     = metric_val,
        metric_unit      = "score",
        message          = msg_val,
        location         = paste0(ds_name, ":", var_name),
        evidence_payload = payload
      )
    }
  }

  # ---- Assemble data.frame ----
  n <- length(rows)
  ev_df <- data.frame(
    run_id           = ctx$run_id,
    study_id         = ctx$study_id,
    asset_type       = vapply(rows, `[[`, character(1), "asset_type"),
    asset_id         = vapply(rows, `[[`, character(1), "asset_id"),
    source_name      = vapply(rows, `[[`, character(1), "source_name"),
    source_version   = vapply(rows, `[[`, character(1), "source_version"),
    indicator_id     = vapply(rows, `[[`, character(1), "indicator_id"),
    indicator_name   = vapply(rows, `[[`, character(1), "indicator_name"),
    indicator_domain = vapply(rows, `[[`, character(1), "indicator_domain"),
    severity         = vapply(rows, `[[`, character(1), "severity"),
    result           = vapply(rows, `[[`, character(1), "result"),
    metric_value     = vapply(rows, `[[`, double(1),    "metric_value"),
    metric_unit      = vapply(rows, `[[`, character(1), "metric_unit"),
    message          = vapply(rows, `[[`, character(1), "message"),
    location         = vapply(rows, `[[`, character(1), "location"),
    evidence_payload = vapply(rows, `[[`, character(1), "evidence_payload"),
    created_at       = Sys.time(),
    stringsAsFactors = FALSE
  )

  cli::cli_alert_info("define_xml_to_evidence: {n} row{?s} generated")
  as_evidence(ev_df, ctx = ctx)
}
