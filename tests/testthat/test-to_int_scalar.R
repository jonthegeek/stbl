# Almost all of this is tested in to_int. Just add quick scalar tests.

test_that("to_int_scalar() allows length-1 ints through", {
  given <- 1L
  expect_identical(to_int_scalar(given), given)
})

test_that("to_int_scalar() provides informative error messages", {
  given <- 1:10
  expect_snapshot(
    to_int_scalar(given),
    error = TRUE
  )

  wrapper <- function(wrapper_val, ...) {
    return(to_int_scalar(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )
})
