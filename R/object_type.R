# object_type was derived from use_standalone("r-lib/rlang",
# "standalone-obj-type.R") but simplified.

#' Identify the class, type, etc of an object
#'
#' Extract the class (or type) of an object for use in error messages.
#'
#' @param x An object to test.
#'
#' @return A length-1 character vector describing the class of the object.
#' @export
#'
#' @examples
#' object_type("a")
#' object_type(1L)
#' object_type(1.1)
#' object_type(mtcars)
#' object_type(rlang::quo(something))
object_type <- function(x) {
  if (missing(x)) {
    return("missing")
  }

  # Anything with a class.
  if (is.object(x)) {
    if (inherits(x, "quosure")) {
      return("quosure")
    }
    return(class(x)[[1L]])
  }

  # Leftovers. Consider adding more cases here, but for now I like the
  # specificity.
  type <- typeof(x)
  return(
    switch(type,
      language = "call",
      closure = "function",
      type
    )
  )
}
