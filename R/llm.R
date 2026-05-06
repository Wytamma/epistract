#' Create an ellmer LLM client backed by Ollama
#'
#' Uses `ollamar` to optionally verify that Ollama is running and to pull a model
#' before returning an `ellmer::chat_ollama()` client object.
#'
#' @param model Ollama model name.
#' @param system_prompt System prompt sent to the model.
#' @param base_url Ollama server URL.
#' @param check_connection Should the local Ollama server be checked first?
#' @param pull Should the model be pulled with `ollamar::pull()` before use?
#' @param ... Additional arguments passed to `ellmer::chat_ollama()`.
#'
#' @return An LLM client object suitable for `ellmer::parallel_chat_structured()`.
#' @export
llm <- function(model = "gemma3:4b",
                system_prompt = paste(
                  "You extract structured fields from epidemiological case investigation reports.",
                  "The most important details are laboratory result, notification, interview timing, symptoms, exposures, travel, contacts, and residence.",
                  "Only return values grounded in the supplied text.",
                  "Use missing values when information is absent."
                ),
                base_url = Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434"),
                check_connection = TRUE,
                pull = FALSE,
                ...) {
  if (isTRUE(check_connection)) {
    ok <- tryCatch({
      ollamar::test_connection()
      TRUE
    }, error = function(cnd) {
      rlang::abort(
        paste(
          "Could not connect to Ollama.",
          "Start the Ollama app or set `base_url` to a reachable server.",
          conditionMessage(cnd)
        )
      )
    })

    if (!isTRUE(ok)) {
      rlang::abort("Ollama connection check did not succeed.")
    }
  }

  if (isTRUE(pull)) {
    ollamar::pull(model)
  }

  ellmer::chat_ollama(
    model = model,
    system_prompt = system_prompt,
    base_url = base_url,
    ...
  )
}