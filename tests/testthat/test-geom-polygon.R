
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

test_that("geom_polygon is closed before munching", {

  df <- data_frame0(
    x = c(1, 1, 4, 4, 2, 2, 3, 3),
    y = c(1, 4, 4, 1, 2, 3, 3, 2),
    sub = c(1, 1, 1, 1, 2, 2, 2, 2)
  )

  p <- ggplot(df, aes(x, y, subgroup = sub)) +
    geom_polygon() +
    xlim(c(0.5, 4.5)) +
    ylim(c(0, 5)) +
    coord_polar()

  built <- ggplot_build(p)
  coord <- built$plot$coordinates
  data  <- built$data[[1]]
  param <- built$layout$panel_params[[1]]

  closed <- coord_munch(coord, data, param, is_closed = TRUE)
  open   <- coord_munch(coord, data, param, is_closed = FALSE)

  p <- ggplot(mapping = aes(x = x, y = y, group = subgroup)) +
    geom_polygon(aes(colour = "closed"), data = closed, fill = NA) +
    geom_polygon(aes(colour = "open"), data = open, fill = NA) +
    theme_void()

  expect_doppelganger("open and closed munched polygons", p)
})
