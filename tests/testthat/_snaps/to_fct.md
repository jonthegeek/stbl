# to_fct() throws errors for bad levels

    Code
      to_fct(letters[1:5], levels = c("a", "c"), to_na = "b")
    Condition
      Error:
      ! All values of `letters[1:5]` must be present in `levels` or `to_na`.
      * Bad values: d and e.

---

    Code
      wrapped_to_fct(letters[1:5], levels = c("a", "c"), to_na = "b")
    Condition
      Error in `wrapped_to_fct()`:
      ! All values of `val` must be present in `levels` or `to_na`.
      * Bad values: d and e.

# to_fct() respects allow_null

    Code
      to_fct(given, allow_null = FALSE)
    Condition
      Error:
      ! `given` must not be <NULL>.

---

    Code
      wrapped_to_fct(given, allow_null = FALSE)
    Condition
      Error in `wrapped_to_fct()`:
      ! `val` must not be <NULL>.

# to_fct() errors for things that can't be coerced

    Code
      to_fct(given)
    Condition
      Error:
      ! Can't coerce `given` <function> to <factor>.

---

    Code
      wrapped_to_fct(given)
    Condition
      Error in `wrapped_to_fct()`:
      ! Can't coerce `val` <function> to <factor>.

---

    Code
      to_fct(given)
    Condition
      Error:
      ! Can't coerce `given` <data.frame> to <factor>.

---

    Code
      wrapped_to_fct(given)
    Condition
      Error in `wrapped_to_fct()`:
      ! Can't coerce `val` <data.frame> to <factor>.

---

    Code
      to_fct(given)
    Condition
      Error:
      ! Can't coerce `given` <list> to <factor>.

---

    Code
      wrapped_to_fct(given)
    Condition
      Error in `wrapped_to_fct()`:
      ! Can't coerce `val` <list> to <factor>.

# to_fct_scalar() provides informative error messages

    Code
      to_fct_scalar(given)
    Condition
      Error:
      ! `given` must be a single <factor>.
      x `given` has 26 values.

---

    Code
      wrapped_to_fct_scalar(given)
    Condition
      Error in `wrapped_to_fct_scalar()`:
      ! `val` must be a single <factor>.
      x `val` has 26 values.

# to_fct_scalar respects allow_zero_length

    Code
      to_fct_scalar(given, allow_zero_length = FALSE)
    Condition
      Error:
      ! `given` must be a single <factor (non-empty)>.
      x `given` has no values.

