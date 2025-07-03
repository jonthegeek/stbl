# stabilize_chr() checks values

    Code
      stabilize_chr(given, regex = pattern)
    Condition
      Error:
      ! `given` must match the regex pattern ^\d{5}(?:[-\s]\d{4})?$
      x Some values do not match.
      * Locations: 1
      * Values: 123456789

---

    Code
      wrapper(given, regex = pattern)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must match the regex pattern ^\d{5}(?:[-\s]\d{4})?$
      x Some values do not match.
      * Locations: 1
      * Values: 123456789

# stabilize_chr() works with complex url regex

    Code
      stabilize_chr("example.com", regex = url_regex)
    Output
      [1] "example.com"

---

    Code
      stabilize_chr(c("example.com", "not a url"), regex = url_regex)
    Condition
      Error:
      ! `c("example.com", "not a url")` must match the regex pattern ^(?:(?:(?:https?|ftp):)?\/\/)?(?:\S+(?::\S*)?@)?(?:(?!(?:10|127)(?:\.\d{1,3}){3})(?!(?:169\.254|192\.168)(?:\.\d{1,3}){2})(?!172\.(?:1[6-9]|2\d|3[0-1])(?:\.\d{1,3}){2})(?:[1-9]\d?|1\d\d|2[01]\d|22[0-3])(?:\.(?:1?\d{1,2}|2[0-4]\d|25[0-5])){2}(?:\.(?:[1-9]\d?|1\d\d|2[0-4]\d|25[0-4]))|(?:(?:[a-z0-9\u00a1-\uffff][a-z0-9\u00a1-\uffff_-]{0,62})?[a-z0-9\u00a1-\uffff]\.)+(?:[a-z\u00a1-\uffff]{2,}\.?))(?::\d{2,5})?(?:[/?#]\S*)?$
      x Some values do not match.
      * Locations: 2
      * Values: not a url

# stabilize_chr() allows for customized error messages

    Code
      stabilize_chr(c("not a url", "example.com"), regex = c(`must be a url.` = url_regex))
    Condition
      Error:
      ! `c("not a url", "example.com")` must be a url.
      x Some values do not match.
      * Locations: 1
      * Values: not a url

# stabilize_chr() works with regex that contains braces

    Code
      stabilize_chr(c("b", "aa"), regex = "a{1,3}")
    Condition
      Error:
      ! `c("b", "aa")` must match the regex pattern a{1,3}
      x Some values do not match.
      * Locations: 1
      * Values: b

# stabilize_chr_scalar() provides informative error messages

    Code
      stabilize_chr_scalar(given)
    Condition
      Error:
      ! `given` must be a single <character>.
      x `given` has 26 values.

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must be a single <character>.
      x `wrapper_val` has 26 values.

# stabilize_chr_scalar() works with regex that contains braces

    Code
      stabilize_chr_scalar("b", regex = "a{1,3}")
    Condition
      Error:
      ! `"b"` must match the regex pattern a{1,3}
      x "b" does not match.

# stabilize_chr() accepts negated regex args

    Code
      stabilize_chr(given, regex = regex)
    Condition
      Error:
      ! `given` must not match the regex pattern c
      x Some values do not match.
      * Locations: 3
      * Values: c

