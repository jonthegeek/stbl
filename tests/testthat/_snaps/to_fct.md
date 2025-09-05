# to_fct errors for things that can't

    Code
      to_fct(given)
    Condition
      Error:
      ! Can't coerce `given` <function> to <factor>.

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! Can't coerce `wrapper_val` <function> to <factor>.

---

    Code
      to_fct(given)
    Condition
      Error:
      ! Can't coerce `given` <data.frame> to <factor>.

---

    Code
      wrapper(given)
    Condition
      Error in `wrapper()`:
      ! Can't coerce `wrapper_val` <data.frame> to <factor>.

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
      ! Can't coerce `wrapper_val` <list> to <factor>.

# to_fct() throws errors for bad levels

    Code
      to_fct(letters[1:5], levels = c("a", "c"), to_na = "b")
    Condition
      Error:
      ! All values of `letters[1:5]` must be present in `levels` or `to_na`.
      * Bad values: d and e.

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

