context("Utilities")

test_that("finite.cases.data.frame", {
  # All finite --------------------------------------------------------------
  expect_identical(finite.cases(data.frame(x = 4)),              TRUE)          # 1x1
  expect_identical(finite.cases(data.frame(x = 4, y = 11)),      TRUE)          # 1x2
  expect_identical(finite.cases(data.frame(x = 4:5)),            c(TRUE, TRUE)) # 2x1
  expect_identical(finite.cases(data.frame(x = 4:5, y = 11:12)), c(TRUE, TRUE)) # 2x2

  # Has one NA --------------------------------------------------------------
  expect_identical(finite.cases(data.frame(x = NA)),                      FALSE)           # 1x1
  expect_identical(finite.cases(data.frame(x = 4, y = NA)),               FALSE)           # 1x2
  expect_identical(finite.cases(data.frame(x = c(4, NA))),                c(TRUE,  FALSE)) # 2x1
  expect_identical(finite.cases(data.frame(x = c(4, NA), y = c(11, NA))), c(TRUE,  FALSE)) # 2x2
  expect_identical(finite.cases(data.frame(x = c(4, NA), y = c(NA, 12))), c(FALSE, FALSE)) # 2x2
  expect_identical(finite.cases(data.frame(x = c(4, 5),  y = c(NA, 12))), c(FALSE, TRUE))  # 2x2

  # Testing NaN and Inf, using miscellaneous data shapes --------------------
  expect_identical(finite.cases(data.frame(x = c(4, NaN))),                c(TRUE, FALSE))
  expect_identical(finite.cases(data.frame(x = Inf)),                      FALSE)
  expect_identical(finite.cases(data.frame(x = c(4, 5), y = c(-Inf, 12))), c(FALSE, TRUE))
})

test_that("add_group", {
  data <- data.frame(f=letters[7:9], x=1:3, y=4:6, group=c(1, -1, 1))
  expect_true(has_groups(add_group(data[2:4])))  # explicit group column
  expect_true(has_groups(add_group(data[1:3])))  # discrete column
  expect_false(has_groups(add_group(data[2:3]))) # no group or discrete column
})
