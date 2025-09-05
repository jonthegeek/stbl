test_that("stabilize_fct() works", {
  expect_identical(stabilize_fct(letters), factor(letters))
})

test_that("stabilize_fct_scalar() works", {
  expect_identical(stabilize_fct_scalar("a"), factor("a"))
})
