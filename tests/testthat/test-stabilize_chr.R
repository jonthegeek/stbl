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

test_that("stabilize_chr() works with complex url regex", {
  url_regex <- r"(^(?:(?:(?:https?|ftp):)?\/\/)?(?:\S+(?::\S*)?@)?(?:(?!(?:10|127)(?:\.\d{1,3}){3})(?!(?:169\.254|192\.168)(?:\.\d{1,3}){2})(?!172\.(?:1[6-9]|2\d|3[0-1])(?:\.\d{1,3}){2})(?:[1-9]\d?|1\d\d|2[01]\d|22[0-3])(?:\.(?:1?\d{1,2}|2[0-4]\d|25[0-5])){2}(?:\.(?:[1-9]\d?|1\d\d|2[0-4]\d|25[0-4]))|(?:(?:[a-z0-9\u00a1-\uffff][a-z0-9\u00a1-\uffff_-]{0,62})?[a-z0-9\u00a1-\uffff]\.)+(?:[a-z\u00a1-\uffff]{2,}\.?))(?::\d{2,5})?(?:[/?#]\S*)?$)"
  expect_snapshot(
    stabilize_chr(
      "example.com",
      regex = url_regex
    )
  )
})
