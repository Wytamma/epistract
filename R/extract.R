#' Build a default extraction prompt
#'
#' @param text Unstructured text to extract from.
#'
#' @return A single prompt string.
#' @export
epi_default_prompt <- function(text) {
  paste(
    "Extract a structured epidemiological case investigation record from the following text.",
    "Prioritise laboratory result, notification date, interview date, symptom onset, symptoms, exposures, travel, contacts, residence, and occupation.",
    "If timing is vague, preserve the vague timing in the appropriate free-text field and normalize dates only when reasonably supported.",
    "Do not infer facts that are not explicitly supported.",
    "Return missing values for absent or unclear details.",
    "Text:",
    text,
    sep = "\n\n"
  )
}

#' Extract epidemiological fields into new columns
#'
#' Applies `ellmer::parallel_chat_structured()` to a target text column in a data
#' frame, then flattens nested structured outputs into dot-notated columns.
#'
#' @param data A data frame or tibble.
#' @param input_col Target column containing unstructured text.
#' @param type An `ellmer` type specification.
#' @param llm An LLM object, typically created with [llm()].
#' @param prompt Either a function taking one text value and returning a prompt,
#'   or a length-1 character string containing a single `%s` placeholder.
#' @param names_sep Separator used when flattening nested outputs.
#' @param keep_input Should the original input column be kept?
#'
#' @return A tibble with extracted columns appended.
#' @export
extract_epi_data <- function(data,
                             input_col,
                             type = type_epi_case_report(),
                             llm,
                             prompt = epi_default_prompt,
                             names_sep = ".",
                             keep_input = TRUE) {
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame or tibble.")
  }

  input_col <- rlang::as_name(rlang::ensym(input_col))

  if (!input_col %in% names(data)) {
    rlang::abort(paste0("Column `", input_col, "` was not found in `data`."))
  }

  text <- data[[input_col]]

  if (!is.character(text)) {
    text <- as.character(text)
  }

  prompts <- build_prompts(text, prompt)
  extracted <- ellmer::parallel_chat_structured(llm, prompts, type = type)

  if (!is.data.frame(extracted)) {
    extracted <- tibble::as_tibble(extracted)
  } else {
    extracted <- tibble::as_tibble(extracted)
  }

  extracted <- get("flatten_epi_columns", mode = "function")(extracted, names_sep = names_sep)

  if (isTRUE(keep_input)) {
    tibble::as_tibble(cbind(data, extracted, stringsAsFactors = FALSE))
  } else {
    keep <- names(data) != input_col
    tibble::as_tibble(cbind(data[keep], extracted, stringsAsFactors = FALSE))
  }
}

build_prompts <- function(text, prompt) {
  if (is.function(prompt)) {
    return(as.list(vapply(text, prompt, character(1), USE.NAMES = FALSE)))
  }

  if (is.character(prompt) && length(prompt) == 1L) {
    return(as.list(vapply(text, function(value) sprintf(prompt, value), character(1), USE.NAMES = FALSE)))
  }

  rlang::abort("`prompt` must be a function or a length-1 character template.")
}
