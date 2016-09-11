context("geom-path")


# Visual tests ------------------------------------------------------------

test_that("geom_path draws correctly", {
  set.seed(1)

  nCategory <- 5
  nItem <- 6
  df <- data.frame(category = rep(LETTERS[1:nCategory], 1, each = nItem),
                   item = paste("Item#", rep(1:nItem, nCategory, each = 1), sep = ''),
                   value = rep(1:nItem, nCategory, each = 1) + runif(nCategory * nItem) * 0.8)

  df2 <- df[c(1, 2, 7, 8, 13, 14, 3:6, 9:12, 15:nrow(df)), ]

  vdiffr::expect_doppelganger(
    ggplot(df) + geom_path(aes(x = value, y = category, group = item)),
    "lines"
  )
  vdiffr::expect_doppelganger(
    ggplot(df2) + geom_path(aes(x = value, y = category, group = item)),
    "lines_with_changed_data_order_should_have_same_appearance"
  )
  vdiffr::expect_doppelganger(
    ggplot(df) + geom_path(aes(x = value, y = category, group = item, colour = item)),
    "lines_colour"
  )
  vdiffr::expect_doppelganger(
    ggplot(df2) + geom_path(aes(x = value, y = category, group = item, colour = item)),
    "lines_colour_with_changed_data_order_should_have_same_appearance"
  )
})
