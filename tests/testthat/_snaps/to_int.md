# to_int() works for NULL

    Code
      to_int(given, allow_null = FALSE)
    Condition
      Error:
      ! `given` must not be <NULL>.

---

    Code
      wrapper(given, allow_null = FALSE)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must not be <NULL>.

# to_int() works for dbls

    Code
      to_int(given)
    Condition
      Error:
      ! Can't convert from `given` <double> to <integer> due to loss of precision.
      * Locations: 4

---

    Code
      to_int(given)
    Condition
      Error:
      ! Can't convert from `given` <double> to <integer> due to loss of precision.
      * Locations: 4

# to_int() works for chrs

    Code
      to_int(given, coerce_character = FALSE)
    Condition
      Error:
      ! Can't coerce `given` <character> to <integer>.

---

    Code
      wrapper(given, coerce_character = FALSE)
    Condition
      Error in `wrapper()`:
      ! Can't coerce `wrapper_val` <character> to <integer>.

---

    Code
      to_int(given)
    Condition
      Error:
      ! `given` <character> must be coercible to <integer>
      x Can't convert some values due to loss of precision.
      * Locations: 4

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` <character> must be coercible to <integer>
      x Can't convert some values due to loss of precision.
      * Locations: 4

---

    Code
      to_int(given)
    Condition
      Error:
      ! `given` <character> must be coercible to <integer>
      x Can't convert some values due to incompatible values.
      * Locations: 4

# to_int() works for complexes

    Code
      to_int(given)
    Condition
      Error:
      ! `given` <complex> must be coercible to <integer>
      x Can't convert some values due to non-zero complex components.
      * Locations: 4

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` <complex> must be coercible to <integer>
      x Can't convert some values due to non-zero complex components.
      * Locations: 4

# to_int() works for factors

    Code
      to_int(given, coerce_factor = FALSE)
    Condition
      Error:
      ! Can't coerce `given` <factor> to <integer>.

---

    Code
      wrapper(given, coerce_factor = FALSE)
    Condition
      Error in `wrapper()`:
      ! Can't coerce `wrapper_val` <factor> to <integer>.

---

    Code
      to_int(given)
    Condition
      Error:
      ! `given` <factor> must be coercible to <integer>
      x Can't convert some values due to incompatible values.
      * Locations: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, ..., 25, and 26

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` <factor> must be coercible to <integer>
      x Can't convert some values due to incompatible values.
      * Locations: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, ..., 25, and 26

# to_int() errors properly for raw, etc

    Code
      to_int(given)
    Condition
      Error:
      ! Can't convert `given` <raw> to <integer>.

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! Can't convert `wrapper_val` <raw> to <integer>.

---

    Code
      to_int(mean)
    Condition
      Error:
      ! `mean` must be a vector, not a function.

