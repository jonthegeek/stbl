test_that(".glue2() works with custom delimiters", {
  local_var <- "test"
  expect_equal(.glue2("A {local_var} message."), "A {local_var} message.")
  expect_equal(.glue2("A [local_var] message."), "A test message.")
})

test_that(".cli_escape() escapes curly braces", {
  expect_equal(.cli_escape("{message}"), "{{message}}")
  expect_equal(.cli_escape("{{message}}"), "{{{{message}}}}")
  expect_equal(.cli_escape("no braces"), "no braces")
})

test_that(".cli_mark() wraps text in cli markup", {
  expect_equal(.cli_mark("text", "val"), "{.val text}")
  expect_equal(.cli_mark("text", "var"), "{.var text}")
})

test_that("%&&% works as expected", {
  expect_null(NULL %&&% "value")
  expect_equal("not null" %&&% "value", "value")
  expect_equal(TRUE %&&% "value", "value")
})

test_that(".find_failures() works", {
  x <- c("a", "b", "c")
  check_fn <- function(vec, val) {
    vec == val
  }

  expect_null(.find_failures(x, NULL, check_fn))
  expect_null(.find_failures(x, "d", check_fn))
  expect_equal(.find_failures(x, "b", check_fn), 2)
  expect_equal(.find_failures(c(x, "b"), "b", check_fn), c(2, 4))
})
