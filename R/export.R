#' Prepare extracted data for CSV or TSV export
#'
#' Converts list-columns produced by [extract_epi_data()] into plain character
#' columns so the result can be written with base functions such as
#' `write.csv()` or `write.table()`.
#'
#' @param data A data frame or tibble.
#' @param vector_sep Separator used for atomic vectors within list-columns.
#' @param field_sep Separator used between named fields in nested objects.
#' @param row_sep Separator used between rows when a list element contains a data
#'   frame.
#' @param empty Value used for empty vectors or missing values. Defaults to an
#'   empty string so missing values are written as empty cells in delimited
#'   exports.
#'
#' @return A tibble with no list-columns.
#' @export
prepare_epi_export <- function(data,
                               vector_sep = "; ",
                               field_sep = " = ",
                               row_sep = " | ",
                               empty = "") {
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame or tibble.")
  }

  out <- data

  for (name in names(out)) {
    if (is.list(out[[name]])) {
      out[[name]] <- vapply(
        out[[name]],
        format_epi_export_value,
        character(1),
        vector_sep = vector_sep,
        field_sep = field_sep,
        row_sep = row_sep,
        empty = empty,
        USE.NAMES = FALSE
      )
    } else if (is.character(out[[name]])) {
      out[[name]] <- normalize_export_atomic(out[[name]], empty = empty)
    }
  }

  tibble::as_tibble(out)
}

#' Write extracted data to a delimited text file
#'
#' @param data A data frame or tibble, typically the output of
#'   [extract_epi_data()].
#' @param file Output file path.
#' @param sep Field separator passed to [utils::write.table()].
#' @param na String to use for missing values when writing the delimited file.
#'   Defaults to an empty string so missing values are written as blank cells.
#' @param row.names Should row names be written?
#' @param qmethod Quoting method passed to [utils::write.table()].
#' @param ... Additional arguments passed to [utils::write.table()].
#'
#' @return Invisibly returns the export-ready data frame.
#' @export
write_epi_delim <- function(data,
                            file,
                            sep = ",",
                            na = "",
                            row.names = FALSE,
                            qmethod = "double",
                            ...) {
  export_data <- prepare_epi_export(data)

  utils::write.table(
    export_data,
    file = file,
    sep = sep,
    na = na,
    row.names = row.names,
    qmethod = qmethod,
    ...
  )

  invisible(export_data)
}

format_epi_export_value <- function(x,
                                    vector_sep = "; ",
                                    field_sep = " = ",
                                    row_sep = " | ",
                                    empty = "") {
  if (is.null(x) || length(x) == 0L) {
    return(empty)
  }

  if (inherits(x, "data.frame")) {
    if (nrow(x) == 0L || ncol(x) == 0L) {
      return(empty)
    }

    rows <- apply(x, 1, function(row) {
      paste(names(row), as.character(row), sep = field_sep, collapse = vector_sep)
    })
    return(paste(rows, collapse = row_sep))
  }

  if (is.list(x) && !is.null(names(x))) {
    values <- vapply(
      x,
      format_epi_export_value,
      character(1),
      vector_sep = vector_sep,
      field_sep = field_sep,
      row_sep = row_sep,
      empty = empty,
      USE.NAMES = FALSE
    )
    values[is_missing_export_value(values)] <- empty
    return(paste(names(x), values, sep = field_sep, collapse = vector_sep))
  }

  if (is.list(x)) {
    values <- vapply(
      x,
      format_epi_export_value,
      character(1),
      vector_sep = vector_sep,
      field_sep = field_sep,
      row_sep = row_sep,
      empty = empty,
      USE.NAMES = FALSE
    )
    values <- values[!is_missing_export_value(values)]

    if (length(values) == 0L) {
      return(empty)
    }

    return(paste(values, collapse = row_sep))
  }

  values <- as.character(x)
  values <- values[!is_missing_export_value(values)]

  if (length(values) == 0L) {
    return(empty)
  }

  paste(values, collapse = vector_sep)
}

normalize_export_atomic <- function(x, empty = NA_character_) {
  out <- as.character(x)
  out[is_missing_export_value(out)] <- empty
  out
}

is_missing_export_value <- function(x) {
  is.na(x) | trimws(x) == ""
}
