test_that("regex_must_match() works as expected", {
  rule <- regex_must_match("^a")
  expect_type(rule, "character")
  expect_named(rule, "must match the regex pattern ^a")
  expect_equal(unname(rule), "^a")
})

test_that("regex_must_match() deals with characters for glue", {
  rule <- regex_must_match("a{1,3}")
  expect_type(rule, "character")
  expect_named(rule, "must match the regex pattern a{{1,3}}")
  expect_equal(unname(rule), "a{1,3}")
})
