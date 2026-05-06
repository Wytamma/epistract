#' Flatten nested extraction columns with dot notation
#'
#' @param data A data frame containing nested objects or df-cols.
#' @param names_sep Separator used between parent and child names.
#'
#' @return A flat tibble.
#' @export
flatten_epi_columns <- function(data, names_sep = ".") {
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame or tibble.")
  }

  if (nrow(data) == 0L) {
    return(tibble::as_tibble(data))
  }

  rows <- lapply(seq_len(nrow(data)), function(i) {
    tibble::as_tibble_row(
      flatten_record(as.list(data[i, , drop = FALSE]), names_sep = names_sep),
      .name_repair = "minimal"
    )
  })

  tibble::as_tibble(do.call(vctrs::vec_rbind, rows))
}

flatten_record <- function(x, prefix = NULL, names_sep = ".") {
  if (inherits(x, "data.frame")) {
    x <- as.list(x)
  }

  out <- list()
  nms <- names(x)

  for (i in seq_along(x)) {
    name <- nms[[i]]
    value <- normalize_nested_value(x[[i]])
    full_name <- paste_name(prefix, name, names_sep = names_sep)

    if (inherits(value, "data.frame")) {
      out <- c(out, flatten_record(as.list(value), prefix = full_name, names_sep = names_sep))
    } else if (is_named_list(value)) {
      out <- c(out, flatten_record(value, prefix = full_name, names_sep = names_sep))
    } else {
      out[[full_name]] <- leaf_value(value)
    }
  }

  out
}

paste_name <- function(prefix, name, names_sep = ".") {
  if (is.null(prefix) || identical(prefix, "")) {
    return(name)
  }

  paste(prefix, name, sep = names_sep)
}

is_named_list <- function(x) {
  is.list(x) && !is.null(names(x)) && any(names(x) != "")
}

normalize_nested_value <- function(x) {
  if (is.list(x) && length(x) == 1L) {
    inner <- x[[1]]

    if (inherits(inner, "data.frame") || is_named_list(inner)) {
      return(inner)
    }
  }

  x
}

leaf_value <- function(x) {
  if (is.null(x)) {
    return(NA)
  }

  if (is.atomic(x) && length(x) == 0L) {
    return(list(x))
  }

  if (is.atomic(x) && length(x) == 1L) {
    return(x)
  }

  list(x)
}
