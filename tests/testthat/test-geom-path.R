context("geom-path")

test_that("keep_mid_true drops leading/trailing FALSE", {
  expect_equal(keep_mid_true(c(F, F)), c(F, F))
  expect_equal(keep_mid_true(c(F, T, F, T, F)), c(F, T, T, T, F))
  expect_equal(keep_mid_true(c(T, T, F, T, F)), c(T, T, T, T, F))
  expect_equal(keep_mid_true(c(F, T, F, T, T)), c(F, T, T, T, T))
})


# Visual tests ------------------------------------------------------------

test_that("geom_path draws correctly", {
  set.seed(1)

  nCategory <- 5
  nItem <- 6
  df <- data.frame(category = rep(LETTERS[1:nCategory], 1, each = nItem),
                   item = paste("Item#", rep(1:nItem, nCategory, each = 1), sep = ''),
                   value = rep(1:nItem, nCategory, each = 1) + runif(nCategory * nItem) * 0.8)

  df2 <- df[c(1, 2, 7, 8, 13, 14, 3:6, 9:12, 15:nrow(df)), ]

  expect_doppelganger("lines",
    ggplot(df) + geom_path(aes(x = value, y = category, group = item))
  )
  expect_doppelganger("lines with changed data order, should have same appearance",
    ggplot(df2) + geom_path(aes(x = value, y = category, group = item))
  )
  expect_doppelganger("lines, colour",
    ggplot(df) + geom_path(aes(x = value, y = category, group = item, colour = item))
  )
  expect_doppelganger("lines, colour, with changed data order, should have same appearance",
    ggplot(df2) + geom_path(aes(x = value, y = category, group = item, colour = item))
  )
})

test_that("NA linetype is dropped with warning", {
  df <- data.frame(x = 1:2, y = 1:2, z = "a")

  # Somehow the warning does not slip through on ggplot_build()
  if (enable_vdiffr) {
    expect_warning(
      expect_doppelganger(
        "NA linetype",
        ggplot(df, aes(x, y)) + geom_path(linetype = NA)
      ),
      "containing missing values"
    )
  }
})
