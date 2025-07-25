% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stabilize_chr.R
\name{.apply_regex_rule}
\alias{.apply_regex_rule}
\title{Apply a single regex rule to a character vector}
\usage{
.apply_regex_rule(rule, x, x_arg, call)
}
\arguments{
\item{rule}{\verb{(length-1 character)} A regex rule (possibly with a \code{name} and
\code{negate} attribute).}

\item{x}{The argument to stabilize.}

\item{x_arg}{\verb{(length-1 character)} An argument name for x. The automatic
value will work in most cases, or pass it through from higher-level
functions to make error messages clearer in unexported functions.}

\item{call}{The execution environment of the call. See the \code{call} argument of
\code{rlang::abort()} for more information.}
}
\value{
A character vector of error messages if the rule fails, otherwise
\code{NULL}.
}
\description{
Apply a single regex rule to a character vector
}
\keyword{internal}
