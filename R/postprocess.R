#' Retrieve post-processing errors from extracted data
#'
#' @param x An object returned by [extract_epi_data()].
#'
#' @return A tibble with columns `row`, `column`, `value`, and `error`.
#' @export
postprocess_errors <- function(x) {
  errors <- attr(x, "epistract_postprocess_errors", exact = TRUE)

  if (is.null(errors)) {
    return(empty_postprocess_errors())
  }

  errors
}

#' Rebuild a partial date string from components
#'
#' Reconstructs an ISO-like character date from separate `year`, `month`, and
#' `day` components. Missing components are replaced with placeholders such as
#' `XXXX` or `XX`.
#'
#' @param year Year component.
#' @param month Month component.
#' @param day Day component.
#' @param year_unknown Placeholder used when year is missing.
#' @param month_unknown Placeholder used when month is missing.
#' @param day_unknown Placeholder used when day is missing.
#'
#' @return A character vector.
#' @export
rebuild_epi_partial_date <- function(year = NA,
                                     month = NA,
                                     day = NA,
                                     year_unknown = "XXXX",
                                     month_unknown = "XX",
                                     day_unknown = "XX") {
  pieces <- vctrs::vec_recycle_common(year = year, month = month, day = day)

  vapply(
    seq_along(pieces$year),
    function(i) {
      rebuilt <- rebuild_partial_date_row(
        year = pieces$year[[i]],
        month = pieces$month[[i]],
        day = pieces$day[[i]],
        year_unknown = year_unknown,
        month_unknown = month_unknown,
        day_unknown = day_unknown
      )
      rebuilt$value
    },
    character(1)
  )
}

#' Rebuild partial date columns in extracted data
#'
#' Looks for flattened `year`, `month`, and `day` columns under one or more
#' prefixes and creates rebuilt ISO-like character date columns such as
#' `XXXX-03-16` when the year is missing.
#'
#' @param data A data frame or tibble.
#' @param prefixes Optional character vector of partial-date prefixes to
#'   rebuild. If `NULL`, prefixes are inferred from columns ending in `.year`,
#'   `.month`, or `.day`.
#' @param output_suffix Optional suffix appended to rebuilt column names. The
#'   default writes the rebuilt value back to the base prefix.
#' @param remove_parts Should the component columns be removed after rebuilding?
#' @param year_unknown Placeholder used when year is missing.
#' @param month_unknown Placeholder used when month is missing.
#' @param day_unknown Placeholder used when day is missing.
#'
#' @return A tibble with rebuilt date columns. Validation issues are attached as
#'   an `epistract_partial_date_errors` attribute and can be retrieved with
#'   [partial_date_errors()].
#' @export
rebuild_epi_partial_dates <- function(data,
                                      prefixes = NULL,
                                      output_suffix = "",
                                      remove_parts = FALSE,
                                      year_unknown = "XXXX",
                                      month_unknown = "XX",
                                      day_unknown = "XX") {
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame or tibble.")
  }

  prefixes <- prefixes %||% infer_partial_date_prefixes(names(data))
  out <- tibble::as_tibble(data)
  errors <- list()

  for (prefix in prefixes) {
    cols <- partial_date_columns(prefix)
    rebuilt <- rebuild_partial_date_columns(
      year = partial_date_column_data(out, cols[["year"]]),
      month = partial_date_column_data(out, cols[["month"]]),
      day = partial_date_column_data(out, cols[["day"]]),
      prefix = prefix,
      year_unknown = year_unknown,
      month_unknown = month_unknown,
      day_unknown = day_unknown
    )

    target <- paste0(prefix, output_suffix)
    out[[target]] <- rebuilt$values
    errors <- c(errors, rebuilt$errors)

    if (isTRUE(remove_parts)) {
      keep <- !names(out) %in% unname(cols)
      out <- out[keep]
    }
  }

  attr(out, "epistract_partial_date_errors") <- bind_partial_date_errors(errors)
  out
}

#' Retrieve partial-date reconstruction errors
#'
#' @param x An object returned by [rebuild_epi_partial_dates()].
#'
#' @return A tibble with columns `row`, `column`, `value`, and `error`.
#' @export
partial_date_errors <- function(x) {
  errors <- attr(x, "epistract_partial_date_errors", exact = TRUE)

  if (is.null(errors)) {
    return(empty_postprocess_errors())
  }

  errors
}

type_with_postprocess <- function(type, postprocess = NULL) {
  attr(type, "epistract_postprocess") <- postprocess
  type
}

postprocess_date_strict <- function(value, format = "%Y-%m-%d") {
  if (length(value) == 0L) {
    return(as.Date(character()))
  }

  x <- as.character(value)
  missing <- is.na(x) | trimws(x) == ""
  out <- as.Date(rep(NA_character_, length(x)))

  if (all(missing)) {
    return(out)
  }

  parsed <- as.Date(x[!missing], format = format)

  if (any(is.na(parsed))) {
    bad_values <- unique(x[!missing][is.na(parsed)])
    rlang::abort(
      paste0(
        "Could not parse date value(s) with format `",
        format,
        "`: ",
        paste(bad_values, collapse = ", ")
      )
    )
  }

  out[!missing] <- parsed
  out
}

collect_type_postprocessors <- function(type, prefix = NULL, names_sep = ".") {
  out <- list()
  processor <- attr(type, "epistract_postprocess", exact = TRUE)

  if (!is.null(processor) && !is.null(prefix)) {
    out[[prefix]] <- processor
  }

  if (inherits(type, "ellmer::TypeArray")) {
    items <- attributes(type)$items

    if (!is.null(items)) {
      out <- utils::modifyList(out, collect_type_postprocessors(items, prefix = prefix, names_sep = names_sep))
    }

    return(out)
  }

  if (inherits(type, "ellmer::TypeObject")) {
    properties <- attributes(type)$properties

    for (name in names(properties)) {
      child_prefix <- paste_name(prefix, name, names_sep = names_sep)
      out <- utils::modifyList(
        out,
        collect_type_postprocessors(properties[[name]], prefix = child_prefix, names_sep = names_sep)
      )
    }
  }

  out
}

merge_postprocess_maps <- function(defaults, postprocess = NULL) {
  if (is.null(postprocess)) {
    return(defaults)
  }

  if (!is.list(postprocess) || is.null(names(postprocess)) || any(names(postprocess) == "")) {
    rlang::abort("`postprocess` must be a named list of functions.")
  }

  invalid <- !vapply(postprocess, is.function, logical(1))

  if (any(invalid)) {
    rlang::abort("Each element of `postprocess` must be a function.")
  }

  utils::modifyList(defaults, postprocess)
}

apply_postprocessors <- function(data, postprocess = NULL) {
  if (is.null(postprocess) || length(postprocess) == 0L) {
    return(list(data = data, errors = empty_postprocess_errors()))
  }

  out <- data
  errors <- list()

  for (name in names(postprocess)) {
    if (!name %in% names(out)) {
      next
    }

    result <- apply_postprocessor_column(out[[name]], postprocess[[name]], column = name)
    out[[name]] <- result$values
    errors <- c(errors, result$errors)
  }

  errors <- if (length(errors) == 0L) {
    empty_postprocess_errors()
  } else {
    tibble::as_tibble(do.call(rbind, errors))
  }

  list(data = tibble::as_tibble(out), errors = errors)
}

apply_postprocessor_column <- function(x, processor, column) {
  values <- if (is.list(x)) x else as.list(x)
  processed <- vector("list", length(values))
  errors <- list()
  failed <- FALSE

  for (i in seq_along(values)) {
    original <- values[[i]]
    result <- tryCatch(
      processor(original),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      failed <- TRUE
      processed[[i]] <- original
      errors[[length(errors) + 1L]] <- data.frame(
        row = i,
        column = column,
        value = summarize_postprocess_value(original),
        error = conditionMessage(result),
        stringsAsFactors = FALSE
      )
    } else {
      processed[[i]] <- result
    }
  }

  if (failed) {
    return(list(values = x, errors = errors))
  }

  simplified <- if (is.list(x)) {
    processed
  } else {
    simplify_scalar_column(processed)
  }

  if (is.null(simplified)) {
    simplified <- processed
  }

  list(values = simplified, errors = errors)
}

summarize_postprocess_value <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }

  if (length(x) == 0L) {
    return("")
  }

  if (is.atomic(x)) {
    return(paste(as.character(x), collapse = "; "))
  }

  paste(capture.output(str(x)), collapse = " ")
}

empty_postprocess_errors <- function() {
  tibble::tibble(
    row = integer(),
    column = character(),
    value = character(),
    error = character()
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

infer_partial_date_prefixes <- function(names) {
  hits <- grep("\\.(year|month|day)$", names, value = TRUE)

  if (length(hits) == 0L) {
    return(character())
  }

  unique(sub("\\.(year|month|day)$", "", hits))
}

partial_date_columns <- function(prefix) {
  c(
    year = paste0(prefix, ".year"),
    month = paste0(prefix, ".month"),
    day = paste0(prefix, ".day")
  )
}

partial_date_column_data <- function(data, name) {
  if (name %in% names(data)) {
    return(data[[name]])
  }

  rep(NA, nrow(data))
}

rebuild_partial_date_columns <- function(year,
                                         month,
                                         day,
                                         prefix,
                                         year_unknown = "XXXX",
                                         month_unknown = "XX",
                                         day_unknown = "XX") {
  pieces <- vctrs::vec_recycle_common(year = year, month = month, day = day)
  values <- character(length(pieces$year))
  errors <- list()

  for (i in seq_along(values)) {
    rebuilt <- rebuild_partial_date_row(
      year = pieces$year[[i]],
      month = pieces$month[[i]],
      day = pieces$day[[i]],
      year_unknown = year_unknown,
      month_unknown = month_unknown,
      day_unknown = day_unknown
    )
    values[[i]] <- rebuilt$value

    if (!is.null(rebuilt$error)) {
      errors[[length(errors) + 1L]] <- data.frame(
        row = i,
        column = prefix,
        value = values[[i]],
        error = rebuilt$error,
        stringsAsFactors = FALSE
      )
    }
  }

  list(values = values, errors = errors)
}

rebuild_partial_date_row <- function(year,
                                     month,
                                     day,
                                     year_unknown = "XXXX",
                                     month_unknown = "XX",
                                     day_unknown = "XX") {
  year_part <- normalize_partial_date_piece(year, width = 4L, unknown = year_unknown, label = "year")
  month_part <- normalize_partial_date_piece(month, width = 2L, unknown = month_unknown, label = "month")
  day_part <- normalize_partial_date_piece(day, width = 2L, unknown = day_unknown, label = "day")

  errors <- c(year_part$error, month_part$error, day_part$error)
  errors <- errors[!is.na(errors)]

  list(
    value = paste(year_part$value, month_part$value, day_part$value, sep = "-"),
    error = if (length(errors) == 0L) NULL else paste(errors, collapse = "; ")
  )
}

normalize_partial_date_piece <- function(x, width, unknown, label) {
  if (length(x) == 0L || is.null(x) || all(is.na(x)) || trimws(as.character(x)[1]) == "") {
    return(list(value = unknown, error = NA_character_))
  }

  raw <- as.character(x)[1]
  numeric <- suppressWarnings(as.integer(raw))

  if (is.na(numeric)) {
    return(list(
      value = unknown,
      error = paste0("Could not interpret `", label, "` value: ", raw)
    ))
  }

  range_error <- switch(
    label,
    month = if (numeric < 1L || numeric > 12L) paste0("Month outside 1-12: ", raw) else NA_character_,
    day = if (numeric < 1L || numeric > 31L) paste0("Day outside 1-31: ", raw) else NA_character_,
    year = if (numeric < 0L || numeric > 9999L) paste0("Year outside 0-9999: ", raw) else NA_character_,
    NA_character_
  )

  list(value = sprintf(paste0("%0", width, "d"), numeric), error = range_error)
}

bind_partial_date_errors <- function(errors) {
  if (length(errors) == 0L) {
    return(empty_postprocess_errors())
  }

  tibble::as_tibble(do.call(rbind, errors))
}
