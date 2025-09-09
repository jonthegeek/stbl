test_that("are_dbl_ish() works for dbls", {
  testthat::expect_identical(
    are_dbl_ish(c(1.0, 2.1, NA, Inf, -Inf)),
    rep(TRUE, 5)
  )
})

test_that("are_dbl_ish() works for ints", {
  testthat::expect_identical(are_dbl_ish(1:10), rep(TRUE, 10))
})

test_that("are_dbl_ish() works for NULL", {
  testthat::expect_identical(are_dbl_ish(NULL), logical(0))
})

test_that("are_dbl_ish() works for logicals", {
  testthat::expect_identical(are_dbl_ish(c(TRUE, FALSE, NA)), rep(TRUE, 3))
})

test_that("are_dbl_ish() works for characters", {
  testthat::expect_identical(
    are_dbl_ish(c("1", "2.0", "Inf", NA)),
    c(TRUE, TRUE, TRUE, TRUE)
  )
  testthat::expect_identical(
    are_dbl_ish(c("a", "")),
    c(FALSE, FALSE)
  )
})

test_that("are_dbl_ish() respects coerce_character", {
  testthat::expect_identical(
    are_dbl_ish(c("1", "2.0"), coerce_character = TRUE),
    c(TRUE, TRUE)
  )
  testthat::expect_identical(
    are_dbl_ish(c("1", "2.0"), coerce_character = FALSE),
    c(FALSE, FALSE)
  )
})

test_that("are_dbl_ish() works for factors", {
  testthat::expect_identical(are_dbl_ish(factor(c(1, 2.2, NA))), rep(TRUE, 3))
  testthat::expect_identical(are_dbl_ish(factor(c("a"))), c(FALSE))
})

test_that("are_dbl_ish() respects coerce_factor", {
  testthat::expect_identical(
    are_dbl_ish(factor(1:2), coerce_factor = TRUE),
    c(TRUE, TRUE)
  )
  testthat::expect_identical(
    are_dbl_ish(factor(1:2), coerce_factor = FALSE),
    c(FALSE, FALSE)
  )
})

test_that("are_dbl_ish() works for complex", {
  testthat::expect_identical(are_dbl_ish(c(1 + 0i, 2.0 + 0i, NA)), rep(TRUE, 3))
  testthat::expect_identical(are_dbl_ish(c(1 + 1i)), c(FALSE))
})

test_that("are_dbl_ish() works for lists", {
  testthat::expect_identical(
    are_dbl_ish(list(1, 2L, "3.3", NA, 4.0)),
    rep(TRUE, 5)
  )
  testthat::expect_identical(
    are_dbl_ish(list("a", NULL, list(1))),
    c(FALSE, FALSE, FALSE)
  )
  testthat::expect_identical(
    are_dbl_ish(list("a", NULL, 1)),
    c(FALSE, FALSE, TRUE)
  )
})

test_that("are_dbl_ish() returns FALSE for non-vectors", {
  testthat::expect_false(are_dbl_ish(mean))
})

test_that("is_dbl_ish() works", {
  testthat::expect_true(is_dbl_ish(1.0))
  testthat::expect_true(is_dbl_ish(c(1, 2.0, NA)))
  testthat::expect_true(is_dbl_ish(NULL))
  testthat::expect_true(is_dbl_ish(list(1, 2L, "3.3")))

  testthat::expect_false(is_dbl_ish("a"))
  testthat::expect_false(is_dbl_ish(list(1, "a")))
})
