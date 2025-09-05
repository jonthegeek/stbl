test_that(".stbl_abort() throws the expected error", {
  expect_error(
    .stbl_abort("A message.", "a_subclass"),
    class = "stbl-error-a_subclass"
  )
  expect_error(
    .stbl_abort("A message.", "a_subclass"),
    class = "stbl-error"
  )
  expect_error(
    .stbl_abort("A message.", "a_subclass"),
    class = "stbl-condition"
  )
  expect_snapshot(
    .stbl_abort("A message.", "a_subclass"),
    error = TRUE
  )
})

test_that(".stbl_abort() uses parent when provided", {
  parent_cnd <- rlang::catch_cnd(cli::cli_abort("parent message"))
  expect_snapshot(
    .stbl_abort("child message", "child_class", parent = parent_cnd),
    error = TRUE
  )
})

test_that(".stbl_abort() passes dots to cli_abort()", {
  expect_error(
    .stbl_abort("A message.", "a_subclass", .internal = TRUE),
    class = "stbl-error-a_subclass"
  )
  expect_snapshot(
    .stbl_abort("A message.", "a_subclass", .internal = TRUE),
    error = TRUE
  )
})

test_that(".stbl_abort() uses message_env when provided", {
  var <- "a locally defined var"
  msg_env <- new.env()
  msg_env$var <- "a custom environment"
  expect_snapshot(
    .stbl_abort(
      "This message comes from {var}.",
      "subclass",
      message_env = msg_env
    ),
    error = TRUE
  )
})

test_that(".compile_error_class() works", {
  expect_equal(
    .compile_error_class("stbl", "error", "my_subclass"),
    "stbl-error-my_subclass"
  )
  expect_equal(
    .compile_error_class("stbl", "error"),
    "stbl-error"
  )
  expect_equal(
    .compile_error_class("stbl", "condition"),
    "stbl-condition"
  )
})

test_that(".stop_cant_coerce() throws the expected error", {
  expect_error(
    .stop_cant_coerce("character", "integer", "my_arg", rlang::current_env()),
    class = .compile_error_class("stbl", "error", "coerce", "integer")
  )
  expect_error(
    .stop_cant_coerce("character", "integer", "my_arg", rlang::current_env()),
    class = .compile_error_class("stbl", "error", "coerce")
  )
  expect_error(
    .stop_cant_coerce("character", "integer", "my_arg", rlang::current_env()),
    class = .compile_error_class("stbl", "error")
  )
  expect_snapshot(
    .stop_cant_coerce("character", "integer", "my_arg", rlang::current_env()),
    error = TRUE
  )
})

test_that(".stop_cant_coerce() uses additional_msg when provided", {
  expect_snapshot(
    .stop_cant_coerce(
      "character",
      "integer",
      "my_arg",
      rlang::current_env(),
      additional_msg = c(x = "An extra message.")
    ),
    error = TRUE
  )
})

test_that(".stop_must() throws the expected error", {
  expect_error(
    .stop_must("must be a foo.", "my_arg", rlang::current_env()),
    class = .compile_error_class("stbl", "error", "must")
  )
  expect_error(
    .stop_must("must be a foo.", "my_arg", rlang::current_env()),
    class = .compile_error_class("stbl", "error")
  )
  expect_snapshot(
    .stop_must("must be a foo.", "my_arg", rlang::current_env()),
    error = TRUE
  )
})

test_that(".stop_must() handles subclasses", {
  expect_error(
    .stop_must(
      "must be a foo.",
      "my_arg",
      rlang::current_env(),
      subclass = "my_custom_class"
    ),
    class = .compile_error_class("stbl", "error", "my_custom_class")
  )
  expect_error(
    .stop_must(
      "must be a foo.",
      "my_arg",
      rlang::current_env(),
      subclass = "my_custom_class"
    ),
    class = .compile_error_class("stbl", "error", "must")
  )
  expect_snapshot(
    .stop_must(
      "must be a foo.",
      "my_arg",
      rlang::current_env(),
      subclass = "my_custom_class"
    ),
    error = TRUE
  )
})

test_that(".stop_must() uses additional_msg when provided", {
  expect_snapshot(
    .stop_must(
      "must be a foo.",
      "my_arg",
      rlang::current_env(),
      additional_msg = c("*" = "Some details.")
    ),
    error = TRUE
  )
})

test_that(".define_main_msg() works", {
  expect_equal(
    .define_main_msg("my_arg", "must be a foo."),
    "{.arg my_arg} must be a foo."
  )
})

test_that(".stop_null() throws the expected error", {
  expect_error(
    .stop_null("my_arg", rlang::current_env()),
    class = .compile_error_class("stbl", "error", "bad_null")
  )
  expect_snapshot(
    .stop_null("my_arg", rlang::current_env()),
    error = TRUE
  )
})

test_that(".stop_null() passes dots", {
  expect_error(
    .stop_null("my_arg", rlang::current_env(), .internal = TRUE),
    class = .compile_error_class("stbl", "error", "bad_null")
  )
  expect_snapshot(
    .stop_null("my_arg", rlang::current_env(), .internal = TRUE),
    error = TRUE
  )
})

test_that(".stop_incompatible() throws the expected error", {
  expect_error(
    .stop_incompatible(
      "character",
      integer(),
      c(FALSE, TRUE, FALSE, TRUE),
      "some reason",
      "my_arg",
      rlang::current_env()
    ),
    class = .compile_error_class("stbl", "error", "incompatible_type")
  )
  expect_error(
    .stop_incompatible(
      "character",
      integer(),
      c(FALSE, TRUE, FALSE, TRUE),
      "some reason",
      "my_arg",
      rlang::current_env()
    ),
    class = .compile_error_class("stbl", "error", "must")
  )
  expect_snapshot(
    .stop_incompatible(
      "character",
      integer(),
      c(FALSE, TRUE, FALSE, TRUE),
      "some reason",
      "my_arg",
      rlang::current_env()
    ),
    error = TRUE
  )
})

test_that(".stop_incompatible() passes dots", {
  expect_error(
    .stop_incompatible(
      "character",
      integer(),
      c(FALSE, TRUE, FALSE, TRUE),
      "some reason",
      "my_arg",
      rlang::current_env(),
      .internal = TRUE
    ),
    class = .compile_error_class("stbl", "error", "incompatible_type")
  )
  expect_snapshot(
    .stop_incompatible(
      "character",
      integer(),
      c(FALSE, TRUE, FALSE, TRUE),
      "some reason",
      "my_arg",
      rlang::current_env(),
      .internal = TRUE
    ),
    error = TRUE
  )
})
