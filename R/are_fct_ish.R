#' Check if an object can be safely coerced to a factor
#'
#' @description
#' `are_fct_ish()` is a vectorized predicate function that checks whether each
#' element of its input can be safely coerced to a factor.
#'
#' `is_fct_ish()` is a scalar predicate function that checks if all elements of
#' its input can be safely coerced to a factor.
#'
#' @inheritParams .shared-params-check
#' @inheritParams .shared-params
#'
#' @returns `are_fct_ish()` returns a logical vector with the same length as the
#'   input. `is_fct_ish()` returns a `length-1 logical` (`TRUE` or `FALSE`) for
#'   the entire vector.
#' @export
are_fct_ish <- function(x, ..., levels = NULL, to_na = character()) {
  UseMethod("are_fct_ish")
}

#' @export
#' @rdname are_fct_ish
is_fct_ish <- function(x, ...) {
  all(are_fct_ish(x, ...))
}

#' @export
#' @rdname are_fct_ish
are_fct_ish.factor <- function(x, ..., levels = NULL, to_na = character()) {
  are_fct_ish(as.character(x), ..., levels = levels, to_na = to_na)
}

#' @export
#' @rdname are_fct_ish
are_fct_ish.character <- function(x, ..., levels = NULL, to_na = character()) {
  !.are_not_fct_ish_chr(x, levels, to_na)
}

#' Check for values that would be lost during factor coercion
#'
#' @inheritParams .shared-params-check
#' @inheritParams .shared-params
#' @returns A logical vector where `TRUE` indicates a failure.
#' @keywords internal
.are_not_fct_ish_chr <- function(x, levels, to_na = character()) {
  if (is.null(levels)) {
    return(rep(FALSE, length(x)))
  }
  if (length(to_na)) {
    x[x %in% to_na] <- NA
  }
  was_na <- is.na(x)
  cast <- factor(x, levels = levels)
  xor(is.na(cast), was_na)
}

#' @export
are_fct_ish.NULL <- function(x, ...) {
  logical(0)
}

#' @export
#' @rdname are_fct_ish
are_fct_ish.default <- function(
  x,
  ...,
  levels = NULL,
  to_na = character(),
  depth = 1
) {
  if (rlang::is_atomic(x)) {
    return(are_fct_ish(as.character(x), ..., levels = levels, to_na = to_na))
  }

  if (!rlang::is_vector(x) || depth != 1) {
    return(FALSE)
  }

  .elements_are_cls_ish(x, are_fct_ish, ..., levels = levels, to_na = to_na)
}
