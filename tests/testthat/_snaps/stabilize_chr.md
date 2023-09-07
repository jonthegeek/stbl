# stabilize_chr() checks values

    Code
      stabilize_chr(given, regex = "^\\d{5}(?:[-\\s]\\d{4})?$")
    Condition
      Error:
      ! `given` must match the provided regex pattern.
      x Some values do not match.
      * Locations: 1

---

    Code
      wrapper(given, regex = "^\\d{5}(?:[-\\s]\\d{4})?$")
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must match the provided regex pattern.
      x Some values do not match.
      * Locations: 1

# stabilize_chr() works with complex url regex

    Code
      stabilize_chr("example.com", regex = url_regex)
    Output
      [1] "example.com"

