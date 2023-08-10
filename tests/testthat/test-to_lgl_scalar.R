test_that("to_lgl_scalar() allows length-1 lgls through", {
  given <- TRUE
  expect_true(to_lgl_scalar(given))
  given <- FALSE
  expect_false(to_lgl_scalar(given))
  given <- NULL
  expect_null(to_lgl_scalar(given))
})

test_that("to_lgl_scalar() provides informative error messages", {
  given <- c(TRUE, FALSE, TRUE)
  expect_snapshot(
    to_lgl_scalar(given),
    error = TRUE
  )

  wrapper <- function(wrapper_val, ...) {
    return(to_lgl_scalar(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )

  given <- "a"
  expect_snapshot(
    to_lgl_scalar(given),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )

  given <- NULL
  expect_snapshot(
    to_lgl_scalar(given, allow_null = FALSE),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, allow_null = FALSE),
    error = TRUE
  )
})
