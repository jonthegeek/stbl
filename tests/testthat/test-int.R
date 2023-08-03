wrapper <- function(wrapper_val, ...) {
  return(to_int(wrapper_val, ...))
}

test_that("to_int() works for ints", {
  given <- 1:1000
  expect_identical(
    to_int(given),
    given
  )
  expect_identical(
    wrapper(given),
    given
  )
  given[[42]] <- NA
  expect_identical(
    to_int(given),
    given
  )
  expect_identical(
    wrapper(given),
    given
  )
  expect_snapshot(
    to_int(given, allow_na = FALSE),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, allow_na = FALSE),
    error = TRUE
  )
})

test_that("to_int() works for NULL", {
  given <- NULL
  expect_identical(
    to_int(given),
    given
  )
  expect_identical(
    wrapper(given),
    given
  )
  expect_snapshot(
    to_int(given, allow_null = FALSE),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, allow_null = FALSE),
    error = TRUE
  )
})

test_that("to_int() works for lgls", {
  given <- rep(c(TRUE, FALSE), 500)
  expected <- as.integer(given)
  expect_identical(
    to_int(given),
    expected
  )
  expect_identical(
    wrapper(given),
    expected
  )
})

test_that("to_int() works for dbls", {
  expected <- 1:1000
  given <- as.double(expected)
  expect_identical(
    to_int(given),
    expected
  )
  expect_identical(
    wrapper(given),
    expected
  )
  given[[42]] <- 1.1
  expect_snapshot(
    to_int(given),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )
  given[[42]] <- Inf
  expect_snapshot(
    to_int(given),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )
})

test_that("to_int() works for chrs", {
  expected <- 1:1000
  given <- as.character(expected)
  expect_identical(
    to_int(given),
    expected
  )
  expect_identical(
    wrapper(given),
    expected
  )
  given[[42]] <- "1.1"
  expect_snapshot(
    to_int(given),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )
  given[[42]] <- "a"
  expect_snapshot(
    to_int(given),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )
})

test_that("to_int() works for complexes", {
  expected <- 1:1000
  given <- as.complex(expected)
  expect_identical(
    to_int(given),
    expected
  )
  expect_identical(
    wrapper(given),
    expected
  )
  given[[42]] <- 1 + 1i
  expect_snapshot(
    to_int(given),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )
})

test_that("to_int() works for hexbins, etc", {
  expected <- 1:1000
  given <- as.hexmode(expected)
  expect_identical(
    to_int(given),
    expected
  )
  expect_identical(
    wrapper(given),
    expected
  )

  given <- as.raw(1:255)
  expect_snapshot(
    to_int(given),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )

  expect_snapshot(
    to_int(mean),
    error = TRUE
  )
  expect_snapshot(
    wrapper(mean),
    error = TRUE
  )
})
