test_that("stabilize_chr() checks values", {
  expect_identical(stabilize_chr("a"), "a")

  given <- "12345-6789"
  pattern <- r"(^\d{5}(?:[-\s]\d{4})?$)"
  expect_identical(
    stabilize_chr(
      given,
      regex = pattern
    ),
    given
  )

  given <- c("123456789", "12345-6789")
  expect_error(
    stabilize_chr(
      given,
      regex = pattern
    ),
    class = .compile_error_class("stbl", "error", "must")
  )
  expect_snapshot(
    stabilize_chr(
      given,
      regex = pattern
    ),
    error = TRUE
  )

  wrapper <- function(wrapper_val, ...) {
    return(stabilize_chr(wrapper_val, ...))
  }
  expect_error(
    wrapper(
      given,
      regex = pattern
    ),
    class = .compile_error_class("stbl", "error", "must")
  )
  expect_snapshot(
    wrapper(
      given,
      regex = pattern
    ),
    error = TRUE
  )
})

test_that("stabilize_chr() works with complex url regex", {
  skip_if_not_installed("stringi")
  url_regex <- r"(^(?:(?:(?:https?|ftp):)?\/\/)?(?:\S+(?::\S*)?@)?(?:(?!(?:10|127)(?:\.\d{1,3}){3})(?!(?:169\.254|192\.168)(?:\.\d{1,3}){2})(?!172\.(?:1[6-9]|2\d|3[0-1])(?:\.\d{1,3}){2})(?:[1-9]\d?|1\d\d|2[01]\d|22[0-3])(?:\.(?:1?\d{1,2}|2[0-4]\d|25[0-5])){2}(?:\.(?:[1-9]\d?|1\d\d|2[0-4]\d|25[0-4]))|(?:(?:[a-z0-9\u00a1-\uffff][a-z0-9\u00a1-\uffff_-]{0,62})?[a-z0-9\u00a1-\uffff]\.)+(?:[a-z\u00a1-\uffff]{2,}\.?))(?::\d{2,5})?(?:[/?#]\S*)?$)"
  expect_snapshot(
    stabilize_chr(
      "example.com",
      regex = url_regex
    )
  )
  expect_error(
    stabilize_chr(
      c("example.com", "not a url"),
      regex = url_regex
    ),
    class = .compile_error_class("stbl", "error", "must")
  )
  expect_snapshot(
    stabilize_chr(
      c("example.com", "not a url"),
      regex = url_regex
    ),
    error = TRUE
  )
})

test_that("stabilize_chr() allows for customized error messages", {
  skip_if_not_installed("stringi")
  url_regex <- r"(^(?:(?:(?:https?|ftp):)?\/\/)?(?:\S+(?::\S*)?@)?(?:(?!(?:10|127)(?:\.\d{1,3}){3})(?!(?:169\.254|192\.168)(?:\.\d{1,3}){2})(?!172\.(?:1[6-9]|2\d|3[0-1])(?:\.\d{1,3}){2})(?:[1-9]\d?|1\d\d|2[01]\d|22[0-3])(?:\.(?:1?\d{1,2}|2[0-4]\d|25[0-5])){2}(?:\.(?:[1-9]\d?|1\d\d|2[0-4]\d|25[0-4]))|(?:(?:[a-z0-9\u00a1-\uffff][a-z0-9\u00a1-\uffff_-]{0,62})?[a-z0-9\u00a1-\uffff]\.)+(?:[a-z\u00a1-\uffff]{2,}\.?))(?::\d{2,5})?(?:[/?#]\S*)?$)"
  expect_error(
    stabilize_chr(
      c("not a url", "example.com"),
      regex = c("must be a url." = url_regex)
    ),
    class = .compile_error_class("stbl", "error", "must")
  )
  expect_snapshot(
    stabilize_chr(
      c("not a url", "example.com"),
      regex = c("must be a url." = url_regex)
    ),
    error = TRUE
  )
})

test_that("stabilize_chr() works with regex that contains braces", {
  expect_error(
    stabilize_chr(c("b", "aa"), regex = "a{1,3}"),
    class = .compile_error_class("stbl", "error", "must")
  )
  expect_snapshot(
    stabilize_chr(c("b", "aa"), regex = "a{1,3}"),
    error = TRUE
  )
})

test_that("stabilize_chr() accepts negated regex args", {
  given <- c("a", "b")
  regex <- "c"
  attr(regex, "negate") <- TRUE
  expect_identical(
    stabilize_chr(given, regex = regex),
    given
  )

  given <- c("a", "b", "c")
  expect_error(
    stabilize_chr(given, regex = regex),
    class = .compile_error_class("stbl", "error", "must")
  )
  expect_snapshot(
    stabilize_chr(given, regex = regex),
    error = TRUE
  )
})

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

test_that("stabilize_chr_scalar() works with regex that contains braces", {
  expect_error(
    stabilize_chr_scalar("b", regex = "a{1,3}"),
    class = .compile_error_class("stbl", "error", "must")
  )
  expect_snapshot(
    stabilize_chr_scalar("b", regex = "a{1,3}"),
    error = TRUE
  )
})

test_that("stabilize_chr() accepts multiple regex rules", {
  rules <- list(
    regex_must_match("a"),
    regex_must_not_match("b")
  )
  given <- c("apple", "avocado")
  expect_identical(
    stabilize_chr(given, regex = rules),
    given
  )
  given <- c("apple", "banana", "boat", "plum")
  expect_error(
    stabilize_chr(given, regex = rules),
    class = .compile_error_class("stbl", "error", "must")
  )
  expect_snapshot(
    stabilize_chr(given, regex = rules),
    error = TRUE
  )
})

test_that("stabilize_chr() works with stringr pattern modifiers", {
  skip_if_not_installed("stringr")

  # fixed()
  expect_identical(
    stabilize_chr("a.b", regex = stringr::fixed("a.b")),
    "a.b"
  )
  expect_error(
    stabilize_chr(c("a.b", "acb"), regex = stringr::fixed("a.b")),
    class = .compile_error_class("stbl", "error", "must")
  )
  expect_snapshot(
    stabilize_chr(c("a.b", "acb"), regex = stringr::fixed("a.b")),
    error = TRUE
  )

  # coll()
  expect_identical(
    stabilize_chr("A", regex = stringr::coll("a", ignore_case = TRUE)),
    "A"
  )
  expect_error(
    stabilize_chr(c("a", "A"), regex = stringr::coll("a")),
    class = .compile_error_class("stbl", "error", "must")
  )
  expect_snapshot(
    stabilize_chr(c("a", "A"), regex = stringr::coll("a")),
    error = TRUE
  )

  # regex()
  expect_identical(
    stabilize_chr("A", regex = stringr::regex("a", ignore_case = TRUE)),
    "A"
  )
  expect_error(
    stabilize_chr(c("A", "B"), regex = stringr::regex("a", ignore_case = TRUE)),
    class = .compile_error_class("stbl", "error", "must")
  )
  expect_snapshot(
    stabilize_chr(c("A", "B"), regex = stringr::regex("a", ignore_case = TRUE)),
    error = TRUE
  )
})
