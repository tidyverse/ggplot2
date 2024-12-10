test_that("finite_cases.data.frame", {
  finite_cases <- function(x) cases(x, is_finite)

  # All finite --------------------------------------------------------------
  expect_true(finite_cases(data_frame(x = 4)))          # 1x1
  expect_true(finite_cases(data_frame(x = 4, y = 11)))          # 1x2
  expect_identical(finite_cases(data_frame(x = 4:5)),            c(TRUE, TRUE)) # 2x1
  expect_identical(finite_cases(data_frame(x = 4:5, y = 11:12)), c(TRUE, TRUE)) # 2x2

  # Has one NA --------------------------------------------------------------
  expect_false(finite_cases(data_frame(x = NA)))           # 1x1
  expect_false(finite_cases(data_frame(x = 4, y = NA)))           # 1x2
  expect_identical(finite_cases(data_frame(x = c(4, NA))),                c(TRUE,  FALSE)) # 2x1
  expect_identical(finite_cases(data_frame(x = c(4, NA), y = c(11, NA))), c(TRUE,  FALSE)) # 2x2
  expect_identical(finite_cases(data_frame(x = c(4, NA), y = c(NA, 12))), c(FALSE, FALSE)) # 2x2
  expect_identical(finite_cases(data_frame(x = c(4, 5),  y = c(NA, 12))), c(FALSE, TRUE))  # 2x2

  # Testing NaN and Inf, using miscellaneous data shapes --------------------
  expect_identical(finite_cases(data_frame(x = c(4, NaN))),                c(TRUE, FALSE))
  expect_false(finite_cases(data_frame(x = Inf)))
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
  expect_length(ggplot_global$x_aes, length(ggplot_global$y_aes))
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
  expect_snapshot_warning(new <- remove_missing(data, finite = TRUE))
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

test_that("vec_rbind0 can combined ordered factors", {

  withr::local_options(lifecycle_verbosity = "warning")

  # Ideally code below throws just 1 warning (the <ordered> and <ordered> one)
  # However, it was technically challenging to reduce the numbers of warnings
  # See #5139 for more details

  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      lifecycle::expect_deprecated(
        {
          test <- vec_rbind0(
            data_frame0(a = factor(c("A", "B"), ordered = TRUE)),
            data_frame0(a = factor(c("B", "C"), ordered = TRUE))
          )
        }
      )
    )
  )

  # Should be <factor> not <ordered/factor>, hence the 'exact'
  expect_s3_class(test$a, "factor", exact = TRUE)
  # Test levels are combined sensibly
  expect_equal(levels(test$a), c("A", "B", "C"))

})

test_that("resolution() gives correct answers", {
  expect_equal(resolution(c(4,  6)), 2)
  expect_equal(resolution(c(4L, 6L)), 1L)
  expect_equal(resolution(mapped_discrete(c(4, 6)), discrete = TRUE), 1L)
  expect_equal(resolution(mapped_discrete(c(4, 6))), 2)
  expect_equal(resolution(c(0, 0)), 1L)
  expect_equal(resolution(c(0.5,  1.5), zero = TRUE), 0.5)

  # resolution has a tolerance
  expect_equal(resolution(c(1, 1 + 1000 * .Machine$double.eps, 2)), 1)
})

test_that("expose/ignore_data() can round-trip a data.frame", {

  # Plain data.frame
  df <- data_frame0(a = 1:3, b = 4:6, c = LETTERS[1:3], d = LETTERS[4:6])
  expect_equal(list(df), .ignore_data(df))
  expect_equal(list(df), .expose_data(df))

  # data.frame with ignored columns
  df <- data_frame0(a = 1:3, b = I(4:6), c = LETTERS[1:3], d = I(LETTERS[4:6]))
  test <- .ignore_data(df)[[1]]
  expect_named(test, c("a", "c", ".ignored"))
  expect_named(test$.ignored, c("b", "d"))

  test <- .expose_data(test)[[1]]
  expect_equal(test, df[, c("a", "c", "b", "d")])

})

test_that("summary method gives a nice summary", {
  # This test isn't important enough to break anything on CRAN
  skip_on_cran()

  p <- ggplot(mpg, aes(displ, hwy, colour = drv)) +
    geom_point() +
    scale_x_continuous() +
    scale_colour_brewer() +
    facet_grid(year ~ cyl)

  expect_snapshot(summary(p))
})
