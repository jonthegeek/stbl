#' Ensure a logical argument meets expectations and is length-1
#'
#' This function is equivalent to [stabilize_lgl()], but it is optimized to
#' check for length-1 logical vectors.
#'
#' @inheritParams stabilize_lgl
#'
#' @return `x`, unless one of the checks fails.
#' @export
#'
#' @examples
#' stabilize_lgl_scalar(TRUE)
#' stabilize_lgl_scalar("TRUE")
#' try(stabilize_lgl_scalar(c(TRUE, FALSE, TRUE)))
#' stabilize_lgl_scalar(NULL)
#' try(stabilize_lgl_scalar(NULL, allow_null = FALSE))
stabilize_lgl_scalar <- function(x,
                                 ...,
                                 allow_null = TRUE,
                                 allow_na = TRUE,
                                 x_arg = rlang::caller_arg(x),
                                 call = rlang::caller_env(),
                                 x_class = object_type(x)) {
  .stabilize_cls_scalar(
    x,
    to_cls_fn = to_lgl,
    allow_null = allow_null,
    allow_na = allow_na,
    x_arg = x_arg,
    call = call,
    x_class = x_class,
    ...
  )
}
