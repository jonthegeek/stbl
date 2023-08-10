#' Ensure a character argument meets expectations
#'
#' Check a character argument to ensure that it meets expectations, coercing it
#' to character where possible. If the argument does not meet the requirements,
#' the user will receive an informative error message. Note that [to_chr()] is a
#' faster version of this function with fewer options.
#'
#' @inheritParams .coerce-params
#' @inheritParams to_chr
#'
#' @return The argument as a character vector.
#' @export
#'
#' @examples
#' stabilize_chr(letters)
#' stabilize_chr(1:10)
#' stabilize_chr(NULL)
#' try(stabilize_chr(NULL, allow_null = FALSE))
#' try(stabilize_chr(c("a", NA), allow_na = FALSE))
#' try(stabilize_chr(letters, min_size = 50))
#' try(stabilize_chr(letters, max_size = 20))
stabilize_chr <- function(x,
                          ...,
                          allow_null = TRUE,
                          allow_na = TRUE,
                          min_size = NULL,
                          max_size = NULL,
                          x_arg = rlang::caller_arg(x),
                          call = rlang::caller_env(),
                          x_class = object_type(x)) {
  .stabilize_cls(
    x,
    to_cls_fn = to_chr,
    allow_null = allow_null,
    allow_na = allow_na,
    min_size = min_size,
    max_size = max_size,
    x_arg = x_arg,
    call = call,
    x_class = x_class,
    ...
  )
}
