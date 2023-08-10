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
