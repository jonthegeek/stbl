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

