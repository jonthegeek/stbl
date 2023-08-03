
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ykwim

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/ykwim)](https://CRAN.R-project.org/package=ykwim)
[![Codecov test
coverage](https://codecov.io/gh/jonthegeek/ykwim/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jonthegeek/ykwim?branch=main)
[![R-CMD-check](https://github.com/jonthegeek/ykwim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jonthegeek/ykwim/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

R is flexible about classes. Variables are not declared with explicit
classes, and arguments of the “wrong” class don’t cause errors until
they explicitly fail at some point in the call stack. It would be
helpful to keep that flexibility from a user standpoint, but to error
informatively and quickly if the inputs will not work for a computation.
The purpose of ykwim is to allow programmers to specify what they want,
and to then see if what the user supplied can work for that purpose.

## Installation

You can install the development version of ykwim from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("jonthegeek/ykwim")
```

## Usage

Coming soon. The general idea is shown below. Note: This code is
currently simulated, but this is the goal.

``` r
# Without ykwim.
my_old_fun <- function(my_arg_name) {
  my_arg_name + 1
}
# Perhaps numbers aren't properly translated from character when the data is
# loaded.
my_old_fun("1")
#> Error in my_arg_name + 1 : non-numeric argument to binary operator

my_fun <- function(my_arg_name) {
  my_arg_name <- ykwim::to_int(my_arg_name)
  my_arg_name + 1
}
my_fun("1")
#> [1] 2

# Failures are reported with helpful errors.
my_fun("1.1")
#> Error in `my_fun()`:
#> ! `x` must be coercible to an integer.
#> ✖ "1.1" is not coercible.
#> Run `rlang::last_trace()` to see where the error occurred.

# The errors help locate issues within vectors.
my_fun(c("1", "2", "3.1", "4", "5.2"))
#> Error in `my_fun()`:
#> ! `x` must be coercible to an integer.
#> ✖ "3.1" (element 3) is not coercible.
#> ✖ "5.2" (element 5) is not coercible.
#> Run `rlang::last_trace()` to see where the error occurred.
```

## Code of Conduct

Please note that the ykwim project is released with a [Contributor Code
of Conduct](https://jonthegeek.github.io/ykwim/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
