wrapper <- function(wrapper_val, ...) {
  return(stabilize_int(wrapper_val, ...))
}

# The _int part of this is tested in to_int.

test_that("stabilize_int returns NULL", {
  expect_identical(
    stabilize_int(NULL),
    NULL
  )
  expect_identical(
    wrapper(NULL),
    NULL
  )
})

test_that("stabilize_int fails and complains about weird args", {
  expect_snapshot(
    stabilize_int(1L, new_arg = "red"),
    error = TRUE
  )
  expect_snapshot(
    wrapper(1L, new_arg = "red"),
    error = TRUE
  )
})

test_that("stabilize_int checks NAs", {
  given <- 1:10
  expect_identical(
    stabilize_int(given, allow_na = FALSE),
    given
  )
  expect_identical(
    wrapper(given, allow_na = FALSE),
    given
  )

  given[c(4, 7)] <- NA
  expect_snapshot(
    stabilize_int(given, allow_na = FALSE),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, allow_na = FALSE),
    error = TRUE
  )
})

test_that("stabilize_int checks size", {
  given <- 1:10
  expect_identical(
    stabilize_int(given, min_size = 1, max_size = 10),
    given
  )
  expect_identical(
    wrapper(given, min_size = 1, max_size = 10),
    given
  )

  expect_snapshot(
    stabilize_int(given, min_size = 11),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, min_size = 11),
    error = TRUE
  )
  expect_snapshot(
    stabilize_int(given, max_size = 4),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, max_size = 4),
    error = TRUE
  )
})

test_that("stabilize_int checks values", {
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
