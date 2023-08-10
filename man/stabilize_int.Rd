% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stabilize_int.R
\name{stabilize_int}
\alias{stabilize_int}
\title{Ensure an integer argument meets expectations}
\usage{
stabilize_int(
  x,
  ...,
  allow_null = TRUE,
  allow_na = TRUE,
  coerce_character = TRUE,
  coerce_factor = TRUE,
  min_size = NULL,
  max_size = NULL,
  min_value = NULL,
  max_value = NULL,
  x_arg = rlang::caller_arg(x),
  call = rlang::caller_env(),
  x_class = object_type(x)
)
}
\arguments{
\item{x}{The argument to stabilize.}

\item{...}{These dots are for future extensions and should be empty.}

\item{allow_null}{Logical. Is NULL an acceptable value?}

\item{allow_na}{Logical. Are NA values ok?}

\item{coerce_character}{Logical. Should character vectors such as "1" and
"2.0" be coerced to integer?}

\item{coerce_factor}{Logical. Should factors with values such as "1" and
"2.0" be coerced to integer? Note that this function uses the character
value from the factor, while \code{\link[=as.integer]{as.integer()}} uses the integer index of the
factor.}

\item{min_size}{Integer. The minimum size of the object. Object size will be
tested using \code{\link[vctrs:vec_size]{vctrs::vec_size()}}.}

\item{max_size}{Integer. The maximum size of the object. Object size will be
tested using \code{\link[vctrs:vec_size]{vctrs::vec_size()}}.}

\item{min_value}{Integer scalar. The lowest allowed value for \code{x}. If \code{NULL}
(default) values are not checked.}

\item{max_value}{Integer scalar. The highest allowed value for \code{x}. If \code{NULL}
(default) values are not checked.}

\item{x_arg}{Character. An argument name for x. The automatic value will work
in most cases, or pass it through from higher-level functions to make error
messages clearer in unexported functions.}

\item{call}{The execution environment of the call. See the \code{call} argument of
\code{rlang::abort()} for more information.}

\item{x_class}{Character. The class name of \code{x} to use in error messages. Use
this if you remove a special class from \code{x} before checking its coercion,
but want the error message to match the original class.}
}
\value{
The argument as an integer.
}
\description{
Check an integer argument to ensure that it meets expectations, coercing it
to integer where possible. If the argument does not meet the requirements,
the user will receive an informative error message. Note that \code{\link[=to_int]{to_int()}} is a
faster version of this function with fewer options.
}
\examples{
stabilize_int(1:10)
stabilize_int("1")
stabilize_int(1 + 0i)
stabilize_int(NULL)
try(stabilize_int(NULL, allow_null = FALSE))
try(stabilize_int(c(1, NA), allow_na = FALSE))
try(stabilize_int(letters))
try(stabilize_int("1", coerce_character = FALSE))
try(stabilize_int(factor(c("1", "a"))))
try(stabilize_int(factor("1"), coerce_factor = FALSE))
try(stabilize_int(1:10, min_value = 3))
try(stabilize_int(1:10, max_value = 7))
}