test_that("to_fct errors for things that can't", {
  wrapper <- function(wrapper_val, ...) {
    return(to_fct(wrapper_val, ...))
  }

  given <- mean
  expect_error(
    to_fct(given),
    class = .compile_error_class("stbl", "error", "coerce", "factor")
  )
  expect_snapshot(to_fct(given), error = TRUE)
  expect_snapshot(wrapper(given), error = TRUE)

  given <- mtcars
  expect_error(
    to_fct(given),
    class = .compile_error_class("stbl", "error", "coerce", "factor")
  )
  expect_snapshot(to_fct(given), error = TRUE)
  expect_snapshot(wrapper(given), error = TRUE)

  given <- list(a = 1, b = 1:5)
  expect_error(
    to_fct(given),
    class = .compile_error_class("stbl", "error", "coerce", "factor")
  )
  expect_snapshot(to_chr(given), error = TRUE)
  expect_snapshot(wrapper(given), error = TRUE)
})

test_that("to_fct() works for fcts", {
  given <- factor(letters)
  expect_identical(to_fct(given), given)

  given[[4]] <- NA
  expect_identical(to_fct(given), given)
})

test_that("to_fct() deals with levels of fcts", {
  given <- factor(c("a", "b"))
  expected <- factor(c("a", NA))
  expect_identical(to_fct(given, levels = "a", to_na = "b"), expected)
})

test_that("to_fct() throws errors for bad levels", {
  wrapper <- function(wrapper_val, ...) {
    return(to_fct(wrapper_val, ...))
  }
  expect_error(
    to_fct(letters[1:5], levels = c("a", "c"), to_na = "b"),
    class = .compile_error_class("stbl", "error", "fct_levels")
  )
  expect_snapshot(
    to_fct(letters[1:5], levels = c("a", "c"), to_na = "b"),
    error = TRUE
  )
  expect_error(
    wrapper(letters[1:5], levels = c("a", "c"), to_na = "b"),
    class = .compile_error_class("stbl", "error", "fct_levels")
  )
})

test_that("to_fct() works for chrs", {
  given <- letters
  expected <- factor(letters)
  expect_identical(to_fct(given), expected)

  given[[4]] <- NA
  expected <- factor(given)
  expect_identical(to_fct(given), expected)
})

test_that("to_fct() works for NULL", {
  wrapper <- function(wrapper_val, ...) {
    return(to_fct(wrapper_val, ...))
  }
  given <- NULL
  expect_identical(to_fct(given), given)
  expect_identical(wrapper(given), given)
  expect_error(
    to_fct(given, allow_null = FALSE),
    class = .compile_error_class("stbl", "error", "bad_null")
  )
  expect_error(
    wrapper(given, allow_null = FALSE),
    class = .compile_error_class("stbl", "error", "bad_null")
  )
})

test_that("to_fct() treats numbers as text", {
  given <- 1:10
  expect_identical(to_fct(given), factor(given))
})
