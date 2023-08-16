test_that("to_lgl() fails with missing value", {
  expect_snapshot(
    to_lgl(),
    error = TRUE
  )
})

test_that("to_lgl() works for lgls", {
  expect_true(to_lgl(TRUE))
  expect_false(to_lgl(FALSE))

  given <- sample(c(TRUE, FALSE), size = 10, replace = TRUE)
  expect_identical(
    to_lgl(given),
    given
  )

  wrapper <- function(wrapper_val, ...) {
    return(to_lgl(wrapper_val, ...))
  }
  expect_identical(
    wrapper(given),
    given
  )

  given[[4]] <- NA
  expect_identical(
    to_lgl(given),
    given
  )
  expect_identical(
    wrapper(given),
    given
  )
})

test_that("to_lgl() works for NULL", {
  wrapper <- function(wrapper_val, ...) {
    return(to_lgl(wrapper_val, ...))
  }

  given <- NULL
  expect_identical(
    to_lgl(given),
    given
  )
  expect_identical(
    wrapper(given),
    given
  )
  expect_snapshot(
    to_lgl(given, allow_null = FALSE),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, allow_null = FALSE),
    error = TRUE
  )
})

test_that("to_lgl() works for integers", {
  wrapper <- function(wrapper_val, ...) {
    return(to_lgl(wrapper_val, ...))
  }

  given <- 1L
  expect_true(to_lgl(given))
  given <- 0L
  expect_false(to_lgl(given))

  # By default, behave the same as as.logical for ints. I don't have the
  # specific use case for anything else yet, so leave this!
  given <- 1:10
  expect_identical(
    to_lgl(given),
    as.logical(given)
  )
})

test_that("to_lgl() works for doubles", {
  wrapper <- function(wrapper_val, ...) {
    return(to_lgl(wrapper_val, ...))
  }

  given <- 1
  expect_true(to_lgl(given))
  given <- 0
  expect_false(to_lgl(given))

  # By default, behave the same as as.logical for dbls. I don't have the
  # specific use case for anything else yet, so leave this!
  given <- c(1, 2, 3.1, 4.4, 5)
  expect_identical(
    to_lgl(given),
    as.logical(given)
  )
})

test_that("to_lgl works for characters", {
  given <- "TRUE"
  expect_true(to_lgl(given))
  given <- "FALSE"
  expect_false(to_lgl(given))

  given <- c(
    "TRUE", "T", "true",
    "FALSE", "F", "false"
  )
  expect_identical(
    to_lgl(given),
    as.logical(given)
  )
  given <- c(
    "TRUE", "T", "true", "t", "TrUe",
    "FALSE", "F", "false", "f", "fAlSe"
  )
  expect_identical(
    to_lgl(given),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )

  expect_snapshot(
    to_lgl(letters),
    error = TRUE
  )

  wrapper <- function(wrapper_val, ...) {
    return(to_lgl(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(letters),
    error = TRUE
  )
})

test_that("to_lgl works for factors", {
  given <- factor("TRUE")
  expect_true(to_lgl(given))
  given <- factor("FALSE")
  expect_false(to_lgl(given))

  given <- factor(c(
    "TRUE", "T", "true",
    "FALSE", "F", "false"
  ))
  expect_identical(
    to_lgl(given),
    as.logical(given)
  )
  given <- factor(c(
    "TRUE", "T", "true", "t", "TrUe",
    "FALSE", "F", "false", "f", "fAlSe"
  ))
  expect_identical(
    to_lgl(given),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )

  given <- factor(letters)
  expect_snapshot(
    to_lgl(given),
    error = TRUE
  )

  wrapper <- function(wrapper_val, ...) {
    return(to_lgl(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )
})

test_that("to_lgl() errors for other things", {
  given <- list(1:10)
  expect_snapshot(
    to_lgl(given),
    error = TRUE
  )

  wrapper <- function(wrapper_val, ...) {
    return(to_lgl(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )
})
