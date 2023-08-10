test_that("stabilize_chr() checks values", {
  expect_identical(stabilize_chr("a"), "a")

  expect_identical(stabilize_chr(letters), letters)

  # given <- letters
  # given[[2]] <- NA
  # expect_identical(stabilize_chr(given), given)
  #
  # expect_snapshot(
  #   stabilize_chr(given, allow_na = FALSE),
  #   error = TRUE
  # )
  # wrapper <- function(wrapper_val, ...) {
  #   return(stabilize_chr(wrapper_val, ...))
  # }
  # expect_snapshot(
  #   wrapper(given, allow_na = FALSE),
  #   error = TRUE
  # )
  #
  # expect_snapshot(
  #   stabilize_chr(given, min_size = 30),
  #   error = TRUE
  # )
  # expect_snapshot(
  #   wrapper(given, min_size = 30),
  #   error = TRUE
  # )
  # expect_snapshot(
  #   stabilize_chr(given, max_size = 15),
  #   error = TRUE
  # )
  # expect_snapshot(
  #   wrapper(given, max_size = 15),
  #   error = TRUE
  # )
})
