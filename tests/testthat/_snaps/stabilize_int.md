# stabilize_int() checks values

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

