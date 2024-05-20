
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stbl

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/stbl)](https://CRAN.R-project.org/package=stbl)
[![Codecov test
coverage](https://codecov.io/gh/jonthegeek/stbl/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jonthegeek/stbl?branch=main)
[![R-CMD-check](https://github.com/jonthegeek/stbl/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jonthegeek/stbl/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

R is flexible about classes. Variables are not declared with explicit
classes, and arguments of the “wrong” class don’t cause errors until
they explicitly fail at some point in the call stack. It would be
helpful to keep that flexibility from a user standpoint, but to error
informatively and quickly if the inputs will not work for a computation.
The purpose of stbl is to allow programmers to specify what they want,
and to then see if what the user supplied can work for that purpose.

## Installation

<div class=".pkgdown-release">

Install the released version of stbl from
[CRAN](https://cran.r-project.org/):

``` r
install.packages("stbl")
```

</div>

<div class=".pkgdown-devel">

Install the development version of stbl from
[GitHub](https://github.com/):

``` r
# install.packages("pak")
pak::pak("jonthegeek/stbl")
```

</div>

## Usage

Use within functions to give meaningful error messages for bad argument
classes.

For example, perhaps you would like to protect against the case where
data is not properly translated from character on load.

### Without stbl:

Without the argument-stabilizers provided in stbl, error messages can be
cryptic, and errors trigger when you might not want them to.

``` r
my_old_fun <- function(my_arg_name) {
  my_arg_name + 1
}
my_old_fun("1")
#> Error in my_arg_name + 1: non-numeric argument to binary operator
```

### With stbl:

stbl helps to ensure that arguments are what you expect them to be.

``` r
my_fun <- function(my_arg_name) {
  my_arg_name <- stbl::to_int(my_arg_name)
  my_arg_name + 1
}
my_fun("1")
#> [1] 2
```

Failures are reported with helpful messages.

``` r
my_fun("1.1")
#> Error in `my_fun()`:
#> ! `my_arg_name` <character> must be coercible to <integer>
#> ✖ Can't convert some values due to loss of precision.
#> • Locations: 1
```

The errors help locate issues within vectors.

``` r
my_fun(c("1", "2", "3.1", "4", "5.2"))
#> Error in `my_fun()`:
#> ! `my_arg_name` <character> must be coercible to <integer>
#> ✖ Can't convert some values due to loss of precision.
#> • Locations: 3 and 5
```

## Code of Conduct

Please note that the stbl project is released with a [Contributor Code
of Conduct](https://jonthegeek.github.io/stbl/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
