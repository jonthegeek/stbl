#' Create a 'must match' regex rule
#'
#' Attach a standardized error message to a `regex` argument, including the
#' regex pattern itself. This error message can be used with [stabilize_chr()]
#' and [stabilize_chr_scalar()].
#'
#' @param regex `(character)` The regular expression pattern.
#'
#' @return The `regex` value with [names()] specifying that the pattern must be
#'   matched.
#' @export
#'
#' @examples
#' regex_must_match("[aeiou]")
regex_must_match <- function(regex) {
  rlang::set_names(
    regex,
    paste("must match the regex pattern", .cli_escape(regex))
  )
}
