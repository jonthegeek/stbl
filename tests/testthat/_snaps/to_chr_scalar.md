# to_chr_scalar() provides informative error messages

    Code
      to_chr_scalar(given)
    Condition
      Error:
      ! `given` must be a single <character>.
      x `given` has 26 values.

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must be a single <character>.
      x `wrapper_val` has 26 values.

---

    Code
      to_chr_scalar(given)
    Condition
      Error:
      ! Can't coerce `given` <list> to <character>.

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! Can't coerce `wrapper_val` <list> to <character>.

---

    Code
      to_chr_scalar(given, allow_null = FALSE)
    Condition
      Error:
      ! `given` must not be <NULL>.

---

    Code
      wrapper(given, allow_null = FALSE)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must not be <NULL>.

# to_chr_scalar rejects length-0 chrs when told to do so

    Code
      to_chr_scalar(given, allow_zero_length = FALSE)
    Condition
      Error:
      ! `given` must be a single <character (non-empty)>.
      x `given` has no values.

