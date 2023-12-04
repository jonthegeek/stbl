# to_fct_scalar() provides informative error messages

    Code
      to_fct_scalar(given)
    Condition
      Error:
      ! `given` must be a single <factor>.
      x `given` has 26 values.

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must be a single <factor>.
      x `wrapper_val` has 26 values.

# to_fct_scalar rejects length-0 fcts when told to do so

    Code
      to_fct_scalar(given, allow_zero_length = FALSE)
    Condition
      Error:
      ! `given` must be a single <factor (non-empty)>.
      x `given` has no values.

