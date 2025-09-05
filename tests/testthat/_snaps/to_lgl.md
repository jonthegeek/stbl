# to_lgl() works for NULL

    Code
      to_lgl(given, allow_null = FALSE)
    Condition
      Error:
      ! `given` must not be <NULL>.

---

    Code
      wrapper(given, allow_null = FALSE)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must not be <NULL>.

# to_lgl works for characters

    Code
      to_lgl(letters)
    Condition
      Error:
      ! `letters` <character> must be coercible to <logical>
      x Can't convert some values due to incompatible values.
      * Locations: 1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, ..., 25, and 26

---

    Code
      wrapper(letters)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` <character> must be coercible to <logical>
      x Can't convert some values due to incompatible values.
      * Locations: 1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, ..., 25, and 26

# to_lgl works for factors

    Code
      to_lgl(given)
    Condition
      Error:
      ! `given` <factor> must be coercible to <logical>
      x Can't convert some values due to incompatible values.
      * Locations: 1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, ..., 25, and 26

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` <factor> must be coercible to <logical>
      x Can't convert some values due to incompatible values.
      * Locations: 1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, ..., 25, and 26

# to_lgl() errors for other things

    Code
      to_lgl(given)
    Condition
      Error:
      ! Can't coerce `given` <list> to <logical>.

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! Can't coerce `wrapper_val` <list> to <logical>.

# to_lgl_scalar() provides informative error messages

    Code
      to_lgl_scalar(given)
    Condition
      Error:
      ! `given` must be a single <logical>.
      x `given` has 3 values.

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must be a single <logical>.
      x `wrapper_val` has 3 values.

---

    Code
      to_lgl_scalar(given)
    Condition
      Error:
      ! `given` <character> must be coercible to <logical>
      x Can't convert some values due to incompatible values.
      * Locations: 1

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` <character> must be coercible to <logical>
      x Can't convert some values due to incompatible values.
      * Locations: 1

---

    Code
      to_lgl_scalar(given, allow_null = FALSE)
    Condition
      Error:
      ! `given` must not be <NULL>.

---

    Code
      wrapper(given, allow_null = FALSE)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must not be <NULL>.

