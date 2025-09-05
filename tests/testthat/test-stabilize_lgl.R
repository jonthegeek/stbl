test_that("stabilize_lgl() checks values", {
  given <- TRUE
  expect_true(stabilize_lgl(given))
  given <- FALSE
  expect_false(stabilize_lgl(given))

  given <- c("TRUE", "FALSE", "true", "fALSE")
  expect_identical(
    stabilize_lgl(given),
    c(TRUE, FALSE, TRUE, FALSE)
  )

  given[[2]] <- NA
  expect_identical(
    stabilize_lgl(given),
    c(TRUE, NA, TRUE, FALSE)
  )

  expect_snapshot(
    stabilize_lgl(given, allow_na = FALSE),
    error = TRUE
  )
  wrapper <- function(wrapper_val, ...) {
    return(stabilize_lgl(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(given, allow_na = FALSE),
    error = TRUE
  )

  expect_snapshot(
    stabilize_lgl(given, min_size = 5),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, min_size = 5),
    error = TRUE
  )
  expect_snapshot(
    stabilize_lgl(given, max_size = 3),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, max_size = 3),
    error = TRUE
  )
})

test_that("stabilize_lgl_scalar() allows length-1 lgls through", {
  expect_true(stabilize_lgl_scalar(TRUE))
})

test_that("stabilize_lgl_scalar() provides informative error messages", {
  given <- c(TRUE, FALSE, TRUE)
  expect_snapshot(
    stabilize_lgl_scalar(given),
    error = TRUE
  )

  wrapper <- function(wrapper_val, ...) {
    return(stabilize_lgl_scalar(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )
})
