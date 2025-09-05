test_that("to_fct_scalar() provides informative error messages", {
  given <- letters
  expect_error(
    to_fct_scalar(given),
    class = .compile_error_class("stbl", "error", "non_scalar")
  )
  expect_error(
    to_fct_scalar(given),
    class = .compile_error_class("stbl", "error", "must")
  )
  expect_snapshot(to_fct_scalar(given), error = TRUE)

  wrapper <- function(wrapper_val, ...) {
    return(to_fct_scalar(wrapper_val, ...))
  }
  expect_error(
    wrapper(given),
    class = .compile_error_class("stbl", "error", "non_scalar")
  )
  expect_snapshot(wrapper(given), error = TRUE)
})

test_that("to_fct_scalar rejects length-0 fcts when told to do so", {
  given <- factor()
  expect_error(
    to_fct_scalar(given, allow_zero_length = FALSE),
    class = .compile_error_class("stbl", "error", "bad_empty")
  )
  expect_error(
    to_fct_scalar(given, allow_zero_length = FALSE),
    class = .compile_error_class("stbl", "error", "must")
  )
  expect_snapshot(
    to_fct_scalar(given, allow_zero_length = FALSE),
    error = TRUE
  )
})

test_that("to_fct_scalar() allows length-1 fcts through", {
  expect_identical(to_fct_scalar("a"), factor("a"))
  expect_identical(to_fct_scalar("a", levels = "a"), factor("a"))
})
