test_that("to_null() works on the happy path", {
  expect_null(to_null(NULL))
})

test_that("to_null() errors when NULL isn't allowed", {
  given <- NULL
  expect_snapshot(
    to_null(given, allow_null = FALSE),
    error = TRUE
  )
  wrapper <- function(wrapper_val, ...) {
    return(to_null(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(given, allow_null = FALSE),
    error = TRUE
  )
})

test_that("to_null() coerces anything to NULL", {
  expect_null(to_null(1L))
  expect_null(to_null(mean))
  expect_null(to_null(TRUE))
  expect_null(to_null(letters))
})

test_that("to_null() errors informatively for weird allow_null values", {
  expect_snapshot(
    to_null(NULL, allow_null = NULL),
    error = TRUE
  )
  expect_snapshot(
    to_null(NULL, allow_null = "fish"),
    error = TRUE
  )
  wrapper <- function(wrapper_val, ...) {
    return(to_null(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(given, allow_null = NULL),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, allow_null = "fish"),
    error = TRUE
  )
})
