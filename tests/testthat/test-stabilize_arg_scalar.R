# Almost all of this is tested in stabilize_arg. Just add quick scalar tests.

test_that("stabilize_arg_scalar() allows length-1 args through", {
  given <- 1L
  expect_identical(stabilize_arg_scalar(given), given)
})

test_that("stabilize_arg_scalar() provides informative error messages", {
  given <- 1:10
  expect_error(
    stabilize_arg_scalar(given),
    class = "stbl_error_non_scalar"
  )
  expect_snapshot(
    stabilize_arg_scalar(given),
    error = TRUE
  )

  wrapper <- function(wrapper_val, ...) {
    return(stabilize_arg_scalar(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )

  given <- NULL
  expect_error(
    stabilize_arg_scalar(given, allow_null = FALSE),
    class = "stbl_error_non_scalar"
  )
  expect_snapshot(
    stabilize_arg_scalar(given, allow_null = FALSE),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, allow_null = FALSE),
    error = TRUE
  )
})

test_that("stabilize_arg_scalar() deals with weird values", {
  given <- NULL
  expect_snapshot(
    stabilize_arg_scalar(given, allow_null = c(TRUE, FALSE)),
    error = TRUE
  )
})
