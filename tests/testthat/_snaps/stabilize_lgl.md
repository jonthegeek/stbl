# stabilize_lgl() checks values

    Code
      stabilize_lgl(given, allow_na = FALSE)
    Condition
      Error:
      ! `given` must not contain NA values.
      * NA locations: 2

---

    Code
      wrapper(given, allow_na = FALSE)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must not contain NA values.
      * NA locations: 2

---

    Code
      stabilize_lgl(given, min_size = 5)
    Condition
      Error:
      ! `given` must have size >= 5.
      x 4 is too small.

---

    Code
      wrapper(given, min_size = 5)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must have size >= 5.
      x 4 is too small.

---

    Code
      stabilize_lgl(given, max_size = 3)
    Condition
      Error:
      ! `given` must have size <= 3.
      x 4 is too big.

---

    Code
      wrapper(given, max_size = 3)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must have size <= 3.
      x 4 is too big.

