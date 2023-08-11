# stabilize_arg() fails and complains about weird args

    Code
      stabilize_arg(1L, new_arg = "red")
    Condition
      Error:
      ! `...` must be empty.
      x Problematic argument:
      * new_arg = "red"

---

    Code
      wrapper(1L, new_arg = "red")
    Condition
      Error in `wrapper()`:
      ! `...` must be empty.
      x Problematic argument:
      * new_arg = "red"

# stabilize_arg() rejects NULLs when asked

    Code
      stabilize_arg(given, allow_null = FALSE)
    Condition
      Error:
      ! `given` must not be <NULL>.

---

    Code
      wrapper(given, allow_null = FALSE)
    Condition
      Error in `wrapper()`:
      ! `x` must not be <NULL>.

# stabilize_arg() checks NAs

    Code
      stabilize_arg(given, allow_na = FALSE)
    Condition
      Error:
      ! `given` must not contain NA values.
      * NA locations: 4 and 7

---

    Code
      wrapper(given, allow_na = FALSE)
    Condition
      Error in `wrapper()`:
      ! `x` must not contain NA values.
      * NA locations: 4 and 7

# stabilize_arg() checks size args

    Code
      stabilize_arg(given, min_size = 2, max_size = 1)
    Condition
      Error:
      ! `min_size` can't be larger than `max_size`.
      * `min_size` = 2
      * `max_size` = 1

---

    Code
      wrapper(given, min_size = 2, max_size = 1)
    Condition
      Error in `wrapper()`:
      ! `min_size` can't be larger than `max_size`.
      * `min_size` = 2
      * `max_size` = 1

# stabilize_arg() checks size

    Code
      stabilize_arg(given, min_size = 11)
    Condition
      Error:
      ! `given` must have size >= 11.
      x 3 is too small.

---

    Code
      wrapper(given, min_size = 11)
    Condition
      Error in `wrapper()`:
      ! `x` must have size >= 11.
      x 3 is too small.

---

    Code
      stabilize_arg(given, max_size = 2)
    Condition
      Error:
      ! `given` must have size <= 2.
      x 3 is too big.

