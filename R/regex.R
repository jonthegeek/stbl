#' Create a regex matching rule
#'
#' Attach a standardized error message to a `regex` argument. By default, the
#' message will be "must match the regex pattern \{regex\}". If the input
#' `regex` has a `negate` attribute set to `TRUE`, the message will instead be
#' "must not match...". This message can be used with [stabilize_chr()] and
#' [stabilize_chr_scalar()].
#'
#' @param regex `(character)` The regular expression pattern.
#'
#' @return The `regex` value with [names()] equal to the generated error
#'   message.
#' @export
#'
#' @examples
#' regex_must_match("[aeiou]")
#'
#' # With negation:
#' regex <- "[aeiou]"
#' attr(regex, "negate") <- TRUE
#' regex_must_match(regex)
regex_must_match <- function(regex) {
  verb <- if (isTRUE(attr(regex, "negate"))) {
    "must not match"
  } else {
    "must match"
  }

  rlang::set_names(
    regex,
    paste(verb, "the regex pattern", .cli_mark(.cli_escape(regex), "val"))
  )
}

#' Create a 'must not match' regex rule
#'
#' Attach a standardized error message to a `regex` argument that specifies
#' that the pattern must *not* be matched. This is a wrapper around
#' [regex_must_match()] that sets the `negate` attribute to `TRUE`.
#'
#' @inheritParams regex_must_match
#'
#' @return The `regex` value with a `negate` attribute and with [names()] equal
#'   to the generated "must not match" error message.
#' @export
#'
#' @examples
#' regex_must_not_match("[aeiou]")
regex_must_not_match <- function(regex) {
  attr(regex, "negate") <- TRUE
  regex_must_match(regex)
}
