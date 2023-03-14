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

test_that("check_required_aesthetics() errors on missing", {
  required_single <- c("x", "y")
  required_bidirectional <- c("x|y", "fill")
  expect_snapshot_error(check_required_aesthetics(required_single, present = "x", name = "test"))
  expect_snapshot_error(check_required_aesthetics(required_single, present = "shape", name = "test"))

  expect_snapshot_error(check_required_aesthetics(required_bidirectional, present = "fill", name = "test"))
  expect_snapshot_error(check_required_aesthetics(required_bidirectional, present = "shape", name = "test"))
})

test_that("remove_missing checks input", {
  expect_snapshot_error(remove_missing(na.rm = 1:5))
})

test_that("characters survive remove_missing", {
  data <- data_frame0(x = c("A", NA))
  expect_warning(
    new <- remove_missing(data, finite = TRUE)
  )
  expect_equal(new, data_frame0(x = "A"))
})

test_that("tolower() and toupper() has been masked", {
  expect_snapshot_error(tolower())
  expect_snapshot_error(toupper())
})

test_that("parse_safe() checks input", {
  expect_snapshot_error(parse_safe(1:5))
})

test_that("width_cm() and height_cm() checks input", {
  expect_snapshot_error(width_cm(letters))
  expect_snapshot_error(height_cm(letters))
})

test_that("cut_*() checks its input and output", {
  expect_snapshot_error(cut_number(1, 10))
  expect_snapshot_error(breaks(1:10, "numbers", nbins = 2, binwidth = 05))
  expect_snapshot_error(cut_width(1:10, 1, center = 0, boundary = 0.5))
})

test_that("interleave() checks the vector lengths", {
  expect_snapshot_error(interleave(1:4, numeric()))
})

test_that("vec_rbind0 can combined ordered factors", {

  withr::local_options(lifecycle_verbosity = "warning")

  # Ideally code below throws just 1 warning (the <ordered> and <ordered> one)
  # However, it was technically challenging to reduce the numbers of warnings
  # See #5139 for more details

  expect_warning(
    expect_warning(
      expect_warning(
        {
          test <- vec_rbind0(
            data_frame0(a = factor(c("A", "B"), ordered = TRUE)),
            data_frame0(a = factor(c("B", "C"), ordered = TRUE))
          )
        },
        "<ordered> and <ordered>", class = "lifecycle_warning_deprecated"
      ),
      "<ordered> and <factor>", class = "lifecycle_warning_deprecated"
    ),
    "<ordered> and <factor>", class = "lifecycle_warning_deprecated"
  )

  # Should be <factor> not <ordered/factor>, hence the 'exact'
  expect_s3_class(test$a, "factor", exact = TRUE)
  # Test levels are combined sensibly
  expect_equal(levels(test$a), c("A", "B", "C"))

})
