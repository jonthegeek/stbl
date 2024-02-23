test_that("to_chr() works for chrs", {
  expect_identical(to_chr("a"), "a")

  given <- letters
  expect_identical(
    to_chr(given),
    given
  )

  wrapper <- function(wrapper_val, ...) {
    return(to_chr(wrapper_val, ...))
  }
  expect_identical(
    wrapper(given),
    given
  )

  given[[4]] <- NA
  expect_identical(
    to_chr(given),
    given
  )
  expect_identical(
    wrapper(given),
    given
  )
})

test_that("to_chr() works for NULL", {
  wrapper <- function(wrapper_val, ...) {
    return(to_chr(wrapper_val, ...))
  }

  given <- NULL
  expect_identical(
    to_chr(given),
    given
  )
  expect_identical(
    wrapper(given),
    given
  )
  expect_snapshot(
    to_chr(given, allow_null = FALSE),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, allow_null = FALSE),
    error = TRUE
  )
})

test_that("to_chr() works for other things", {
  given <- 1:10
  expect_identical(
    to_chr(given),
    as.character(given)
  )
  given <- given + 0.1
  expect_identical(
    to_chr(given),
    as.character(given)
  )
  given <- c(TRUE, FALSE, TRUE)
  expect_identical(
    to_chr(given),
    as.character(given)
  )
})

test_that("to_chr() tries to flatten lists", {
  expect_identical(
    to_chr(list("a", "b")),
    c("a", "b")
  )
  expect_identical(
    to_chr(list(1, 2)),
    c("1", "2")
  )
  expect_identical(
    to_chr(list("a")),
    c("a")
  )
})

test_that("to_chr() fails gracefully for weird cases", {
  wrapper <- function(wrapper_val, ...) {
    return(to_chr(wrapper_val, ...))
  }

  given <- mean
  expect_snapshot(
    to_chr(given),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )

  given <- mtcars
  expect_snapshot(
    to_chr(given),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )

  given <- list(a = 1, b = 1:5)
  expect_snapshot(
    to_chr(given),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )
})
