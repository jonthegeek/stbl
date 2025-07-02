test_that(".is_allowed_null() checks whether value is NULL and ok", {
  wrapper <- function(wrapper_val, ...) {
    return(.is_allowed_null(wrapper_val, ...))
  }

  given <- NULL
  expect_true(.is_allowed_null(given))
  expect_true(wrapper(given))
  expect_false(.is_allowed_null(given, allow_null = FALSE))
  expect_false(wrapper(given, allow_null = FALSE))
})
