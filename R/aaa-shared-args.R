#' Shared parameters
#'
#' These parameters are used in multiple coercion functions. They are defined
#' here to make them easier to import and to find.
#'
#' @param ... These dots are for future extensions and should be empty.
#' @param allow_na Logical. Are NA values ok?
#' @param allow_null Logical. Is NULL an acceptable value?
#' @param min_size Integer. The minimum size of the object. Object size will be
#'   tested using [vctrs::vec_size()].
#' @param max_size Integer. The maximum size of the object. Object size will be
#'   tested using [vctrs::vec_size()].
#' @param x The argument to stabilize.
#' @param x_arg Character. An argument name for x. The automatic value will work
#'   in most cases, or pass it through from higher-level functions to make error
#'   messages clearer in unexported functions.
#' @param x_class Character. The class name of `x` to use in error messages. Use
#'   this if you remove a special class from `x` before checking its coercion,
#'   but want the error message to match the original class.
#' @param call The execution environment of the call. See the `call` argument of
#'   `rlang::abort()` for more information.
#'
#' @name .coerce-params
#' @keywords internal
NULL
