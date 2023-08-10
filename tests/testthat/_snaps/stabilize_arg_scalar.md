# stabilize_arg_scalar() provides informative error messages

    Code
      stabilize_arg_scalar(given)
    Condition
      Error:
      ! `given` must be a single <integer>.
      x `given` has 10 values.

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must be a single <integer>.
      x `wrapper_val` has 10 values.

---

    Code
      stabilize_arg_scalar(given, allow_null = FALSE)
    Condition
      Error:
      ! `given` must be a single <non-NULL>.
      x `given` has no values.

---

    Code
      wrapper(given, allow_null = FALSE)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must be a single <non-NULL>.
      x `wrapper_val` has no values.

