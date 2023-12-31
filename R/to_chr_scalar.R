#' Coerce an argument to a length-1 character vector
#'
#' This function wraps [to_chr()], adding a quick check to confirm that the
#' input contains a single value.
#'
#' @inheritParams to_chr
#' @inheritParams .coerce-params
#'
#' @return A character vector equivalent to `x`.
#' @export
#'
#' @examples
#' to_chr_scalar("a")
#' try(to_chr_scalar(letters))
to_chr_scalar <- function(x,
                          allow_null = TRUE,
                          allow_zero_length = TRUE,
                          x_arg = caller_arg(x),
                          call = caller_env(),
                          x_class = object_type(x)) {
  .to_cls_scalar(
    x,
    is_rlang_cls_scalar = is_scalar_character,
    to_cls_fn = to_chr,
    allow_null = allow_null,
    allow_zero_length = allow_zero_length,
    x_arg = x_arg,
    call = call,
    x_class = x_class
  )
}
