test_that("stabilize_int() checks values", {
  wrapper <- function(wrapper_val, ...) {
    return(stabilize_int(wrapper_val, ...))
  }

  given <- 1:10
  expect_identical(
    stabilize_int(given, min_value = 1, max_value = 10),
    given
  )
  expect_identical(
    wrapper(given, min_value = 1, max_value = 10),
    given
  )

  expect_snapshot(
    stabilize_int(given, min_value = 11),
    error = TRUE,
    cnd_class = TRUE
  )
  expect_snapshot(
    wrapper(given, min_value = 11),
    error = TRUE,
    cnd_class = TRUE
  )
  expect_snapshot(
    stabilize_int(given, max_value = 4),
    error = TRUE,
    cnd_class = TRUE
  )
  expect_snapshot(
    wrapper(given, max_value = 4),
    error = TRUE,
    cnd_class = TRUE
  )
})

test_that("stabilize_int_scalar() allows length-1 ints through", {
  given <- 1L
  expect_identical(stabilize_int_scalar(given), given)
})

test_that("stabilize_int_scalar() provides informative error messages", {
  given <- 1:10
  expect_snapshot(
    stabilize_int_scalar(given),
    error = TRUE,
    cnd_class = TRUE
  )

  wrapper <- function(wrapper_val, ...) {
    return(stabilize_int_scalar(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(given),
    error = TRUE,
    cnd_class = TRUE
  )
})
