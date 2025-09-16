# stbl (development version)

* `to_*()` functions now handle lists containing length-1 lists (etc) consistently, flattening when no information will be lost (#128).
* `vignette("stbl")` provides an overview of the package and its functions (#42).
* `to_fct()` now lists the acceptable values when a conversion fails to make it easier for users to understand what went wrong (#67).
* `to_lgl()` now converts character numbers such as "0" and "1" to `FALSE` and `TRUE` respectively (#30).
* Functions for `double` vectors are now available: `are_dbl_ish()`, `is_dbl_ish()`, `to_dbl()`,  `to_dbl_scalar()`,  `stabilize_dbl()`, and `stabilize_dbl_scalar()` (#23).
* A new group of predicate functions are now available. Use `is_chr_ish()`, `is_fct_ish()`, `is_int_ish()`, and `is_lgl_ish()` to check if an object can be coerced to a specific type, and `are_chr_ish()`, `are_fct_ish()`, `are_int_ish()`, and `are_lgl_ish()` to check each element of a given vector (#93).
* Error messages are now clearer and more standardized throughout the package (#95).
* `stabilize_chr()` supports `stringr::regex()`, `stringr::fixed()`, and `stringr::coll()` patterns (#87).
* `stabilize_chr()` can now use new helpers `regex_must_match()` and `regex_must_not_match()` to describe regex patterns for more useful error messages (#52, #85, #86, #89).
* The purpose of and vision for this package are now more clearly described in documentation (#56, #77).

# stbl 0.1.1

* Update formatting in DESCRIPTION and examples.

# stbl 0.1.0

* Initial CRAN submission.
