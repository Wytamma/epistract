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

repeated_prefixes_from_type <- function(type, prefix = NULL, names_sep = ".") {
  prefixes <- character()

  if (inherits(type, "ellmer::TypeArray")) {
    if (!is.null(prefix)) {
      prefixes <- c(prefixes, prefix)
    }

    items <- attributes(type)$items

    if (!is.null(items)) {
      prefixes <- c(
        prefixes,
        repeated_prefixes_from_type(items, prefix = prefix, names_sep = names_sep)
      )
    }

    return(unique(prefixes))
  }

  if (inherits(type, "ellmer::TypeObject")) {
    properties <- attributes(type)$properties

    if (length(properties) == 0L) {
      return(prefixes)
    }

    for (name in names(properties)) {
      child_prefix <- paste_name(prefix, name, names_sep = names_sep)
      prefixes <- c(
        prefixes,
        repeated_prefixes_from_type(properties[[name]], prefix = child_prefix, names_sep = names_sep)
      )
    }
  }

  unique(prefixes)
}

simplify_scalar_list_columns <- function(data,
                                         repeated_prefixes = character(),
                                         names_sep = ".") {
  out <- data

  for (name in names(out)) {
    if (!is.list(out[[name]]) || is_repeated_path(name, repeated_prefixes, names_sep = names_sep)) {
      next
    }

    simplified <- simplify_scalar_column(out[[name]])

    if (!is.null(simplified)) {
      out[[name]] <- simplified
    }
  }

  tibble::as_tibble(out)
}

flatten_record <- function(x, prefix = NULL, names_sep = ".", from_df = FALSE) {
  if (inherits(x, "data.frame")) {
    x <- as.list(x)
    from_df <- TRUE
  }

  out <- list()
  nms <- names(x)

  for (i in seq_along(x)) {
    name <- nms[[i]]
    value <- normalize_nested_value(x[[i]])
    full_name <- paste_name(prefix, name, names_sep = names_sep)

    if (inherits(value, "data.frame")) {
      out <- c(out, flatten_record(value, prefix = full_name, names_sep = names_sep, from_df = TRUE))
    } else if (is_named_list(value)) {
      out <- c(out, flatten_record(value, prefix = full_name, names_sep = names_sep, from_df = from_df))
    } else {
      out[[full_name]] <- leaf_value(value, from_df = from_df)
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

leaf_value <- function(x, from_df = FALSE) {
  if (is.null(x)) {
    return(NA)
  }

  if (isTRUE(from_df)) {
    return(list(x))
  }

  if (is.atomic(x) && length(x) == 0L) {
    return(list(x))
  }

  if (is.atomic(x) && length(x) == 1L) {
    return(x)
  }

  list(x)
}

is_repeated_path <- function(name, repeated_prefixes, names_sep = ".") {
  if (length(repeated_prefixes) == 0L) {
    return(FALSE)
  }

  any(vapply(repeated_prefixes, function(prefix) {
    identical(name, prefix) || startsWith(name, paste0(prefix, names_sep))
  }, logical(1)))
}

simplify_scalar_column <- function(x) {
  if (length(x) == 0L) {
    return(NULL)
  }

  if (!all(vapply(x, is_scalar_like_value, logical(1)))) {
    return(NULL)
  }

  non_missing <- Filter(function(value) !(is.null(value) || length(value) == 0L), x)

  if (length(non_missing) == 0L) {
    return(NULL)
  }

  ptype <- tryCatch(
    do.call(vctrs::vec_ptype_common, non_missing),
    error = function(...) NULL
  )

  if (is.null(ptype)) {
    return(NULL)
  }

  values <- lapply(x, scalar_value_or_missing, ptype = ptype)

  tryCatch(
    do.call(vctrs::vec_c, values),
    error = function(...) NULL
  )
}

is_scalar_like_value <- function(x) {
  is.null(x) || (is.atomic(x) && length(x) <= 1L)
}

scalar_value_or_missing <- function(x, ptype) {
  if (is.null(x) || length(x) == 0L) {
    return(vctrs::vec_init(ptype, 1L))
  }

  x
}
