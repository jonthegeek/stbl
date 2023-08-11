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

