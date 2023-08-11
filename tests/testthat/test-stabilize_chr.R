# Just test one simple + special case(s).

test_that("stabilize_chr() checks values", {
  expect_identical(stabilize_chr("a"), "a")

  given <- "12345-6789"
  expect_identical(
    stabilize_chr(
      given,
      regex = r"(^\d{5}(?:[-\s]\d{4})?$)"
    ),
    given
  )

  given <- "123456789"
  expect_snapshot(
    stabilize_chr(
      given,
      regex = r"(^\d{5}(?:[-\s]\d{4})?$)"
    ),
    error = TRUE
  )

  wrapper <- function(wrapper_val, ...) {
    return(stabilize_chr(wrapper_val, ...))
  }
  expect_snapshot(
    wrapper(
      given,
      regex = r"(^\d{5}(?:[-\s]\d{4})?$)"
    ),
    error = TRUE
  )
})
