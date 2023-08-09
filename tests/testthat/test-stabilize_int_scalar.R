# Almost all of this is tested in stabilize_int Just add quick scalar tests.

test_that("stabilize_int_scalar() allows length-1 ints through", {
  given <- 1L
  expect_identical(stabilize_int_scalar(given), given)
})

test_that("stabilize_int_scalar() provides informative error messages", {
  given <- 1:10
  expect_snapshot(
    stabilize_int_scalar(given),
    error = TRUE
  )

  wrapper <- function(wrapper_val, ...) {
    return(stabilize_int_scalar(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )
})
