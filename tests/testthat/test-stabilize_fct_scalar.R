# Just need to verify that the function exists.

test_that("stabilize_fct_scalar() works", {
  expect_identical(stabilize_fct_scalar("a"), factor("a"))
})
