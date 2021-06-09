context("geom-path")

test_that("keep_mid_true drops leading/trailing FALSE", {
  expect_equal(keep_mid_true(c(F, F)), c(F, F))
  expect_equal(keep_mid_true(c(F, T, F, T, F)), c(F, T, T, T, F))
  expect_equal(keep_mid_true(c(T, T, F, T, F)), c(T, T, T, T, F))
  expect_equal(keep_mid_true(c(F, T, F, T, T)), c(F, T, T, T, T))
})


# Tests on stairstep() ------------------------------------------------------------

test_that("stairstep() does not error with too few observations", {
  df <- data_frame(x = 1, y = 1)
  expect_silent(stairstep(df))
})

test_that("stairstep() exists with error when an invalid `direction` is given", {
  df <- data_frame(x = 1:3, y = 1:3)
  expect_error(stairstep(df, direction="invalid"))
})

test_that("stairstep() output is correct for direction = 'vh'", {
  df <- data_frame(x = 1:3, y = 1:3)
  stepped_expected <- data_frame(x = c(1L, 1L, 2L, 2L, 3L), y = c(1L, 2L, 2L, 3L, 3L))
  stepped <- stairstep(df, direction = "vh")
  expect_equal(stepped, stepped_expected)
})

test_that("stairstep() output is correct for direction = 'hv'", {
  df <- data_frame(x = 1:3, y = 1:3)
  stepped_expected <- data_frame(x = c(1L, 2L, 2L, 3L, 3L), y = c(1L, 1L, 2L, 2L, 3L))
  stepped <- stairstep(df, direction = "hv")
  expect_equal(stepped, stepped_expected)
})

test_that("stairstep() output is correct for direction = 'mid'", {
  df <- data_frame(x = 1:3, y = 1:3)
  stepped_expected <- data_frame(x = c(1, 1.5, 1.5, 2.5, 2.5, 3), y = c(1L, 1L, 2L, 2L, 3L, 3L))
  stepped <- stairstep(df, direction = "mid")
  expect_equal(stepped, stepped_expected)
})


# Visual tests ------------------------------------------------------------

test_that("geom_path draws correctly", {
  set.seed(1)

  nCategory <- 5
  nItem <- 6
  df <- data_frame(category = rep(LETTERS[1:nCategory], 1, each = nItem),
                   item = paste("Item#", rep(1:nItem, nCategory, each = 1), sep = ''),
                   value = rep(1:nItem, nCategory, each = 1) + runif(nCategory * nItem) * 0.8)

  df2 <- df[c(1, 2, 7, 8, 13, 14, 3:6, 9:12, 15:nrow(df)), ]

  expect_doppelganger("lines",
    ggplot(df) + geom_path(aes(x = value, y = category, group = item))
  )
  expect_doppelganger("lines, changed order, should have same appearance",
    ggplot(df2) + geom_path(aes(x = value, y = category, group = item))
  )
  expect_doppelganger("lines, colour",
    ggplot(df) + geom_path(aes(x = value, y = category, group = item, colour = item))
  )
  expect_doppelganger("lines, colour, changed order, should have same appearance",
    ggplot(df2) + geom_path(aes(x = value, y = category, group = item, colour = item))
  )
})

test_that("NA linetype is dropped with warning", {
  df <- data_frame(x = 1:2, y = 1:2, z = "a")

  expect_warning(
    expect_doppelganger(
      "NA linetype",
      ggplot(df, aes(x, y)) + geom_path(linetype = NA)
    ),
    "containing missing values"
  )
})
