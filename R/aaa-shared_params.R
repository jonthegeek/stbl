#' Shared parameters
#'
#' These parameters are used in multiple functions. They are defined here to
#' make them easier to import and to find.
#'
#' @param ... These dots are for future extensions and should be empty.
#' @param allow_na `(length-1 logical)` Are NA values ok?
#' @param allow_null `(length-1 logical)` Is NULL an acceptable value?
#' @param allow_zero_length `(length-1 logical)` Are zero-length vectors
#'   acceptable?
#' @param call `(environment)` The execution environment to mention as the
#'   source of error messages.
#' @param message_env (`environment`) The execution environment to use to
#'   evaluate variables in error messages.
#' @param min_size `(length-1 integer)` The minimum size of the object. Object
#'   size will be tested using [vctrs::vec_size()].
#' @param min_value `(length-1 integer)` The lowest allowed value for `x`. If
#'   `NULL` (default) values are not checked.
#' @param max_size `(length-1 integer)` The maximum size of the object. Object
#'   size will be tested using [vctrs::vec_size()].
#' @param max_value `(length-1 integer)` The highest allowed value for `x`. If
#'   `NULL` (default) values are not checked.
#' @param parent A parent condition, as you might create during a
#'   [rlang::try_fetch()]. See [rlang::abort()] for additional information.
#' @param regex `(character, list, or stringr_pattern)` One or more optional
#'   regular expressions to test against the values of `x`. This can be a
#'   character vector, a list of character vectors, or a pattern object from the
#'   \{stringr\} package (e.g., `stringr::fixed("a.b")`). The default error
#'   message for non-matching values will include the pattern itself (see
#'   [regex_must_match()]). To provide a custom message, supply a named
#'   character vector where the value is the regex pattern and the name is the
#'   message that should be displayed. To check that a pattern is *not* matched,
#'   attach a `negate` attribute set to `TRUE`. If a complex regex pattern
#'   throws an error, try installing the stringi package.
#' @param subclass (`character`) Class(es) to assign to the error. Will be
#'   prefixed by "stbl-error-".
#' @param x The argument to stabilize.
#' @param x_arg `(length-1 character)` An argument name for x. The automatic
#'   value will work in most cases, or pass it through from higher-level
#'   functions to make error messages clearer in unexported functions.
#' @param x_class `(length-1 character)` The class name of `x` to use in error
#'   messages. Use this if you remove a special class from `x` before checking
#'   its coercion, but want the error message to match the original class.
#'
#' @name .shared-params
#' @keywords internal
NULL
