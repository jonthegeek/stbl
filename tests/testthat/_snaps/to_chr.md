# to_chr() works for NULL

    Code
      to_chr(given, allow_null = FALSE)
    Condition
      Error:
      ! `given` must not be <NULL>.

---

    Code
      wrapper(given, allow_null = FALSE)
    Condition
      Error in `wrapper()`:
      ! `wrapper_val` must not be <NULL>.

# to_chr() fails gracefully for weird cases

    Code
      to_chr(given)
    Condition
      Error:
      ! Can't coerce `given` <function> to <character>.

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! Can't coerce `wrapper_val` <function> to <character>.

---

    Code
      to_chr(given)
    Condition
      Error:
      ! Can't coerce `given` <data.frame> to <character>.

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! Can't coerce `wrapper_val` <data.frame> to <character>.

---

    Code
      to_chr(given)
    Condition
      Error:
      ! Can't coerce `given` <list> to <character>.

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! Can't coerce `wrapper_val` <list> to <character>.

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

