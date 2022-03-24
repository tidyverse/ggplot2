test_that("finite_cases.data.frame", {
  finite_cases <- function(x) cases(x, is_finite)

  # All finite --------------------------------------------------------------
  expect_identical(finite_cases(data_frame(x = 4)),              TRUE)          # 1x1
  expect_identical(finite_cases(data_frame(x = 4, y = 11)),      TRUE)          # 1x2
  expect_identical(finite_cases(data_frame(x = 4:5)),            c(TRUE, TRUE)) # 2x1
  expect_identical(finite_cases(data_frame(x = 4:5, y = 11:12)), c(TRUE, TRUE)) # 2x2

  # Has one NA --------------------------------------------------------------
  expect_identical(finite_cases(data_frame(x = NA)),                      FALSE)           # 1x1
  expect_identical(finite_cases(data_frame(x = 4, y = NA)),               FALSE)           # 1x2
  expect_identical(finite_cases(data_frame(x = c(4, NA))),                c(TRUE,  FALSE)) # 2x1
  expect_identical(finite_cases(data_frame(x = c(4, NA), y = c(11, NA))), c(TRUE,  FALSE)) # 2x2
  expect_identical(finite_cases(data_frame(x = c(4, NA), y = c(NA, 12))), c(FALSE, FALSE)) # 2x2
  expect_identical(finite_cases(data_frame(x = c(4, 5),  y = c(NA, 12))), c(FALSE, TRUE))  # 2x2

  # Testing NaN and Inf, using miscellaneous data shapes --------------------
  expect_identical(finite_cases(data_frame(x = c(4, NaN))),                c(TRUE, FALSE))
  expect_identical(finite_cases(data_frame(x = Inf)),                      FALSE)
  expect_identical(finite_cases(data_frame(x = c(4, 5), y = c(-Inf, 12))), c(FALSE, TRUE))
})

test_that("add_group", {
  data <- data_frame(f=letters[7:9], x=1:3, y=4:6, group=c(1, -1, 1))
  expect_true(has_groups(add_group(data[2:4])))  # explicit group column
  expect_true(has_groups(add_group(data[1:3])))  # discrete column
  expect_false(has_groups(add_group(data[2:3]))) # no group or discrete column
})

test_that("find_args behaves correctly", {
  test_fun <- function(arg1, arg2 = FALSE, ...) {
    find_args(...)
  }
  # Missing args are removed
  expect_false("arg1" %in% names(test_fun()))
  # Ellipsis is not an element
  expect_false("..." %in% names(test_fun()))
  # Args are added
  expect_true(all(c("arg1", "arg2", "arg3") %in% names(test_fun(arg1 = 1, arg2 = 1, arg3 = 1))))
  # Defaults are overwritten
  expect_true(test_fun(arg2 = TRUE)$arg2)
})

test_that("parse_safe works with simple expressions", {
  expect_equal(
    parse_safe(c("", " ", "     ")),
    expression(NA, NA, NA)
  )

  expect_equal(
    parse_safe(c("A", "B", "C")),
    expression(A, B, C)
  )

  expect_equal(
    parse_safe(c("alpha", "", "gamma", " ")),
    expression(alpha, NA, gamma, NA)
  )

  expect_equal(
    parse_safe(c(NA, "a", NA, "alpha")),
    expression(NA, a, NA, alpha)
  )
})

test_that("parse_safe works with multi expressions", {
  expect_equal(
    parse_safe(c(" \n", "\n ", " \n  \n  ")),
    expression(NA, NA, NA)
  )

  expect_equal(
    parse_safe(c("alpha ~ beta", "beta \n gamma", "")),
    expression(alpha ~ beta, beta, NA)
  )

  expect_equal(
    parse_safe(c("alpha ~ beta", " ", "integral(f(x) * dx, a, b)")),
    expression(alpha ~ beta, NA, integral(f(x) * dx, a, b))
  )

  expect_equal(
    parse_safe(c(NA, 1, 2, "a \n b")),
    expression(NA, 1, 2, a)
  )
})

test_that("x and y aesthetics have the same length", {
  expect_equal(length(ggplot_global$x_aes), length(ggplot_global$y_aes))
})
