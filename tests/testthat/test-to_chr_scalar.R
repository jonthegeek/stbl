test_that("to_chr_scalar() allows length-1 chrs through", {
  expect_identical(
    to_chr_scalar("a"),
    "a"
  )
  expect_identical(
    to_chr_scalar("b"),
    "b"
  )
})

test_that("to_chr_scalar() provides informative error messages", {
  given <- letters
  expect_snapshot(
    to_chr_scalar(given),
    error = TRUE
  )

  wrapper <- function(wrapper_val, ...) {
    return(to_chr_scalar(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )

  given <- list(a = 1:10)
  expect_snapshot(
    to_chr_scalar(given),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )

  given <- NULL
  expect_snapshot(
    to_chr_scalar(given, allow_null = FALSE),
    error = TRUE
  )
  expect_snapshot(
    wrapper(given, allow_null = FALSE),
    error = TRUE
  )
})
