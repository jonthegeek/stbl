% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to_lgl_scalar.R
\name{to_lgl_scalar}
\alias{to_lgl_scalar}
\title{Coerce an argument to a length-1 logical vector}
\usage{
to_lgl_scalar(
  x,
  allow_null = TRUE,
  allow_zero_length = TRUE,
  x_arg = caller_arg(x),
  call = caller_env(),
  x_class = object_type(x)
)
}
\arguments{
\item{x}{The argument to stabilize.}

\item{allow_null}{Logical. Is NULL an acceptable value?}

\item{allow_zero_length}{Logical. Are zero-length vectors acceptable?}

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
A logical vector equivalent to \code{x}.
}
\description{
This function wraps \code{\link[=to_lgl]{to_lgl()}}, adding a quick check to confirm that the
input contains a single value.
}
\examples{
to_lgl_scalar("TRUE")
try(to_lgl_scalar(c(TRUE, FALSE)))
}
