test_that("stabilize_chr_scalar() allows length-1 chrs through", {
  expect_identical(stabilize_chr_scalar("a"), "a")
})

test_that("stabilize_chr_scalar() provides informative error messages", {
  given <- letters
  expect_snapshot(
    stabilize_chr_scalar(given),
    error = TRUE
  )

  wrapper <- function(wrapper_val, ...) {
    return(stabilize_chr_scalar(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(given),
    error = TRUE
  )
})
