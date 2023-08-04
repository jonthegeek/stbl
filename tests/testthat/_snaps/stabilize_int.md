# stabilize_int fails and complains about weird args

    Code
      stabilize_int(1L, new_arg = "red")
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

# stabilize_int checks NAs

    Code
      stabilize_int(given, allow_na = FALSE)
    Condition
      Error:
      ! `given` must not contain NA values.
      * NA locations: 4 and 7

---

    Code
      wrapper(given, allow_na = FALSE)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must not contain NA values.
      * NA locations: 4 and 7

# stabilize_int checks size

    Code
      stabilize_int(given, min_size = 11)
    Condition
      Error:
      ! `given` must have size >= 11.
      x 10 is too small.

---

    Code
      wrapper(given, min_size = 11)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must have size >= 11.
      x 10 is too small.

---

    Code
      stabilize_int(given, max_size = 4)
    Condition
      Error:
      ! `given` must have size <= 4.
      x 10 is too big.

---

    Code
      wrapper(given, max_size = 4)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must have size <= 4.
      x 10 is too big.

# stabilize_int checks values

    Code
      stabilize_int(given, min_value = 11)
    Condition
      Error:
      ! Values of `given` must be >= 11.
      x Values are too low at locations 1, 2, 3, 4, 5, 6, 7, 8, 9, and 10.

---

    Code
      wrapper(given, min_value = 11)
    Condition
      Error in `wrapper()`:
      ! Values of `wrapper_val` must be >= 11.
      x Values are too low at locations 1, 2, 3, 4, 5, 6, 7, 8, 9, and 10.

---

    Code
      stabilize_int(given, max_value = 4)
    Condition
      Error:
      ! Values of `given` must be <= 4.
      x Values are too high at locations 5, 6, 7, 8, 9, and 10.

---

    Code
      wrapper(given, max_value = 4)
    Condition
      Error in `wrapper()`:
      ! Values of `wrapper_val` must be <= 4.
      x Values are too high at locations 5, 6, 7, 8, 9, and 10.

