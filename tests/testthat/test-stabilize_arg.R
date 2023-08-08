test_that("stabilize_arg() returns its inputs for default settings", {
  given <- 1:10
  expect_identical(stabilize_arg(given), given)
  expect_identical(stabilize_arg(letters), letters)
  given <- given + 0.1
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
  expect_snapshot(
    {
      stabilize_arg(given, allow_null = FALSE)
    },
    error = TRUE
  )
  expect_snapshot(
    {
      wrapper(given, allow_null = FALSE)
    },
    error = TRUE
  )
})

test_that("stabilize_arg() checks NAs", {
  wrapper <- function(x, ...) {
    stabilize_arg(x, ...)
  }
  given <- 1:10
  expect_identical(
    stabilize_arg(given, allow_na = FALSE),
    given
  )
  expect_identical(
    wrapper(given, allow_na = FALSE),
    given
  )

  given[c(4, 7)] <- NA
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
  expect_no_error(stabilize_arg(given, min_size = 1, max_size = 1))

  expect_snapshot(
    stabilize_arg(given, min_size = 2, max_size = 1),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, min_size = 2, max_size = 1),
    error = TRUE
  )
})

test_that("stabilize_arg() checks size", {
  wrapper <- function(x, ...) {
    stabilize_arg(x, ...)
  }

  given <- 1:10
  expect_identical(
    stabilize_arg(given, min_size = 1, max_size = 10),
    given
  )
  expect_identical(
    wrapper(given, min_size = 1, max_size = 10),
    given
  )

  expect_snapshot(
    stabilize_arg(given, min_size = 11),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, min_size = 11),
    error = TRUE
  )
  expect_snapshot(
    stabilize_arg(given, max_size = 4),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, max_size = 4),
    error = TRUE
  )
})
