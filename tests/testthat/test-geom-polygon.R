
# Visual tests ------------------------------------------------------------

skip_if(utils::packageVersion('grid') < "3.6")
test_that("geom_polygon draws correctly", {

  tbl <- data_frame(
    x = c(
      0, 10, 10, 0,
      20, 30, 30, 20,
      22, 28, 28, 22
    ),
    y = c(
      0, 0, 10, 10,
      20, 20, 30, 30,
      22, 22, 28, 28
    ),
    group = c(rep(1, 4), rep(2, 8)),
    subgroup = c(rep(1, 8), rep(2, 4))
  )

  p <- ggplot(tbl, aes(x, y, group = group, subgroup = subgroup)) +
    geom_polygon()

  expect_doppelganger("basic polygon plot", p)
})
