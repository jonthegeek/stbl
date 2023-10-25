test_that("stabilize_arg() returns its inputs for default settings", {
  given <- 1:2
  expect_identical(stabilize_arg(given), given)
  expect_identical(stabilize_arg(NULL), NULL)
})

test_that("stabilize_arg() fails and complains about weird args", {
  wrapper <- function(x, ...) {
    stabilize_arg(x, ...)
  }
  expect_snapshot(
    stabilize_arg(1L, new_arg = "red"),
    error = TRUE
  )
  expect_snapshot(
    wrapper(1L, new_arg = "red"),
    error = TRUE
  )
})

test_that("stabilize_arg() rejects NULLs when asked", {
  wrapper <- function(x, ...) {
    stabilize_arg(x, ...)
  }
  given <- NULL
  expect_error(
    stabilize_arg(given, allow_null = FALSE),
    class = "stbl_error_bad_null"
  )
  expect_snapshot(
    stabilize_arg(given, allow_null = FALSE),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, allow_null = FALSE),
    error = TRUE
  )
})

test_that("stabilize_arg() checks NAs", {
  wrapper <- function(x, ...) {
    stabilize_arg(x, ...)
  }
  given <- 1:8
  expect_identical(stabilize_arg(given, allow_na = FALSE), given)

  given[c(4, 7)] <- NA
  expect_error(
    stabilize_arg(given, allow_na = FALSE),
    class = "stbl_error_bad_na"
  )
  expect_snapshot(
    stabilize_arg(given, allow_na = FALSE),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, allow_na = FALSE),
    error = TRUE
  )
})

test_that("stabilize_arg() checks size args", {
  wrapper <- function(x, ...) {
    stabilize_arg(x, ...)
  }

  given <- TRUE
  expect_true(stabilize_arg(given, min_size = 1, max_size = 1))

  expect_error(
    stabilize_arg(given, min_size = 2, max_size = 1),
    class = "stbl_error_size_x_vs_y"
  )
  expect_snapshot(
    stabilize_arg(given, min_size = 2, max_size = 1),
    error = TRUE
  )
  expect_error(
    wrapper(given, min_size = 2, max_size = 1),
    class = "stbl_error_size_x_vs_y"
  )
  expect_snapshot(
    wrapper(given, min_size = 2, max_size = 1),
    error = TRUE
  )
})

test_that("stabilize_arg() checks size", {
  given <- 1:3
  expect_identical(
    stabilize_arg(given, min_size = 1, max_size = 10),
    given
  )

  expect_error(
    stabilize_arg(given, min_size = 11),
    class = "stbl_error_size_too_small"
  )
  expect_snapshot(
    stabilize_arg(given, min_size = 11),
    error = TRUE
  )
  wrapper <- function(x, ...) {
    stabilize_arg(x, ...)
  }
  expect_error(
    wrapper(given, min_size = 11),
    class = "stbl_error_size_too_small"
  )
  expect_snapshot(
    wrapper(given, min_size = 11),
    error = TRUE
  )
  expect_error(
    stabilize_arg(given, max_size = 2),
    class = "stbl_error_size_too_large"
  )
  expect_snapshot(
    stabilize_arg(given, max_size = 2),
    error = TRUE
  )
})
