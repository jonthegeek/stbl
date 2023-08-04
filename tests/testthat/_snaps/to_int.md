# to_int() works for NULL

    Code
      to_int(given, allow_null = FALSE)
    Condition
      Error:
      ! `given` can't be <NULL>.

---

    Code
      wrapper(given, allow_null = FALSE)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` can't be <NULL>.

# to_int() works for dbls

    Code
      to_int(given)
    Condition
      Error:
      ! Can't convert from `given` <double> to <integer> due to loss of precision.
      * Locations: 4

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! Can't convert from `wrapper_val` <double> to <integer> due to loss of precision.
      * Locations: 4

---

    Code
      to_int(given)
    Condition
      Error:
      ! Can't convert from `given` <double> to <integer> due to loss of precision.
      * Locations: 4

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! Can't convert from `wrapper_val` <double> to <integer> due to loss of precision.
      * Locations: 4

# to_int() works for chrs

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

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` <character> must be coercible to <integer>
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
      to_int(given)
    Condition
      Error:
      ! `given` <character> must be coercible to <integer>
      x Can't convert some values due to incompatible values.
      * Locations: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, ..., 25, and 26

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` <character> must be coercible to <integer>
      x Can't convert some values due to incompatible values.
      * Locations: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, ..., 25, and 26

# to_int() works for hexbins, etc

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

---

    Code
      wrapper(mean)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must be a vector, not a function.

