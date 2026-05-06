#' epistract: Epidemiological Data Extraction from Unstructured Text
#'
#' `epistract` provides structured extraction helpers for epidemiological case
#' investigation narratives, especially enteric and foodborne disease reports
#' that include laboratory results, symptom timelines, exposure histories,
#' travel, and close contacts.
#'
#' The package combines `ellmer` structured outputs with Ollama-backed local
#' models and returns dataframe-friendly results with flattened dot-notated
#' columns.
#'
#' Main entry points include [llm()], [type_epi_case_report()], and
#' [extract_epi_data()].
#'
#' @docType package
#' @name epistract
"_PACKAGE"
