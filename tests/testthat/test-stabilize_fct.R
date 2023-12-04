# There isn't really anything special in this. Just do something so we can see
# if the function disappears.

test_that("stabilize_fct() works", {
  expect_identical(stabilize_fct(letters), factor(letters))
})
