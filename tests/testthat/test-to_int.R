test_that("to_int() works for ints", {
  given <- 1:10
  expect_identical(to_int(given), given)
})

test_that("to_int() works for NULL", {
  given <- NULL
  expect_identical(to_int(given), given)
  expect_snapshot(
    to_int(given, allow_null = FALSE),
    error = TRUE
  )
  wrapper <- function(wrapper_val, ...) {
    return(to_int(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(given, allow_null = FALSE),
    error = TRUE
  )
})

test_that("to_int() works for lgls", {
  given <- c(TRUE, FALSE)
  expected <- as.integer(given)
  expect_identical(to_int(given), expected)
})

test_that("to_int() works for dbls", {
  expected <- 1:10
  given <- as.double(expected)
  expect_identical(to_int(given), expected)

  given[[4]] <- 1.1
  expect_snapshot(
    to_int(given),
    error = TRUE
  )
  given[[4]] <- Inf
  expect_snapshot(
    to_int(given),
    error = TRUE
  )
})

test_that("to_int() works for chrs", {
  expected <- 1:10
  given <- as.character(expected)
  expect_identical(to_int(given), expected)
  expect_snapshot(
    to_int(given, coerce_character = FALSE),
    error = TRUE
  )
  wrapper <- function(wrapper_val, ...) {
    return(to_int(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(given, coerce_character = FALSE),
    error = TRUE
  )

  given[[4]] <- "1.1"
  expect_snapshot(
    to_int(given),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )
  given[[4]] <- "a"
  expect_snapshot(
    to_int(given),
    error = TRUE
  )
})

test_that("to_int() works for complexes", {
  expected <- 1:10
  given <- as.complex(expected)
  expect_identical(to_int(given), expected)
  given[[4]] <- 1 + 1i
  expect_snapshot(
    to_int(given),
    error = TRUE
  )
  wrapper <- function(wrapper_val, ...) {
    return(to_int(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )
})

test_that("to_int() works for factors", {
  expected <- c(1L, 3L, 5L, 7L)
  given <- factor(expected)
  expect_identical(to_int(given), expected)
  expect_snapshot(
    to_int(given, coerce_factor = FALSE),
    error = TRUE
  )
  wrapper <- function(wrapper_val, ...) {
    return(to_int(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(given, coerce_factor = FALSE),
    error = TRUE
  )
  given <- factor(letters)
  expect_snapshot(
    to_int(given),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )
})

test_that("to_int() errors properly for raw, etc", {
  given <- as.raw(1:10)
  expect_snapshot(
    to_int(given),
    error = TRUE
  )
  wrapper <- function(wrapper_val, ...) {
    return(to_int(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )

  expect_snapshot(
    to_int(mean),
    error = TRUE
  )
})
