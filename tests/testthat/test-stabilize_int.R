# Most of the _int part of this is tested in to_int.
# Most of the rest is tested in stabilize_arg.
# Just test the int-specific, special stuff.

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
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, min_value = 11),
    error = TRUE
  )
  expect_snapshot(
    stabilize_int(given, max_value = 4),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, max_value = 4),
    error = TRUE
  )
})
