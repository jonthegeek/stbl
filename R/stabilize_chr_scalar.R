#' Ensure a character argument meets expectations and is length-1
#'
#' This function is equivalent to [stabilize_chr()], but it is optimized to
#' check for length-1 character vectors.
#'
#' @inheritParams stabilize_chr
#' @inheritParams .coerce-params
#'
#' @return `x`, unless one of the checks fails.
#' @export
#'
#' @examples
#' stabilize_chr_scalar(TRUE)
#' stabilize_chr_scalar("TRUE")
#' try(stabilize_chr_scalar(c(TRUE, FALSE, TRUE)))
#' stabilize_chr_scalar(NULL)
#' try(stabilize_chr_scalar(NULL, allow_null = FALSE))
stabilize_chr_scalar <- function(x,
                                 ...,
                                 allow_null = TRUE,
                                 allow_zero_length = TRUE,
                                 allow_na = TRUE,
                                 regex = NULL,
                                 x_arg = caller_arg(x),
                                 call = caller_env(),
                                 x_class = object_type(x)) {
  .stabilize_cls_scalar(
    x,
    to_cls_scalar_fn = to_chr_scalar,
    check_cls_value_fn = .check_value_chr,
    check_cls_value_fn_args = list(regex = regex),
    allow_null = allow_null,
    allow_zero_length = allow_zero_length,
    allow_na = allow_na,
    x_arg = x_arg,
    call = call,
    x_class = x_class,
    ...
  )
}
