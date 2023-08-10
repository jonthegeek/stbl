# to_null() errors when NULL isn't allowed

    Code
      to_null(given, allow_null = FALSE)
    Condition
      Error:
      ! `given` must not be <NULL>.

---

    Code
      wrapper(given, allow_null = FALSE)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must not be <NULL>.

# to_null() errors informatively for weird allow_null values

    Code
      to_null(NULL, allow_null = NULL)
    Condition
      Error:
      ! `allow_null` must not be <NULL>.

---

    Code
      to_null(NULL, allow_null = "fish")
    Condition
      Error:
      ! `allow_null` <character> must be coercible to <logical>
      x Can't convert some values due to incompatible values.
      * Locations: 1

---

    Code
      wrapper(given, allow_null = NULL)
    Condition
      Error in `wrapper()`:
      ! `allow_null` must not be <NULL>.

---

    Code
      wrapper(given, allow_null = "fish")
    Condition
      Error in `wrapper()`:
      ! `allow_null` <character> must be coercible to <logical>
      x Can't convert some values due to incompatible values.
      * Locations: 1

