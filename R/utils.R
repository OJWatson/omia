validate_required_cols <- function(data, cols, arg = "data") {
  if (!is.data.frame(data)) {
    rlang::abort(paste0("`", arg, "` must be a data frame."))
  }

  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(
      paste0(
        "Missing required columns in `", arg, "`: ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
}

as_date_safe <- function(x, col_name) {
  if (inherits(x, "Date")) {
    return(x)
  }

  out <- as.Date(x)
  if (any(is.na(out) & !is.na(x))) {
    rlang::abort(
      paste0("Column `", col_name, "` could not be fully converted to Date.")
    )
  }

  out
}

validate_nonnegative_numeric <- function(x, name) {
  if (!is.numeric(x)) {
    rlang::abort(paste0("`", name, "` must be numeric."))
  }
  if (any(!is.finite(x), na.rm = TRUE)) {
    rlang::abort(paste0("`", name, "` must contain finite values."))
  }
  if (any(x < 0, na.rm = TRUE)) {
    rlang::abort(paste0("`", name, "` must be non-negative."))
  }
}
