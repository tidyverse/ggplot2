test_that("geom_sf() determines the legend type automatically", {
  skip_if_not_installed("sf")
  if (packageVersion("sf") < "0.5.3") skip("Need sf 0.5.3")

  mp <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_multipoint(rbind(c(1,1), c(2,2), c(3,3)))),
    v = "a")

  s1 <- rbind(c(0,3),c(0,4),c(1,5),c(2,5))
  s2 <- rbind(c(0.2,3), c(0.2,4), c(1,4.8), c(2,4.8))
  s3 <- rbind(c(0,4.4), c(0.6,5))

  mls <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_multilinestring(list(s1,s2,s3))),
    v = "a")

  p1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
  p2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
  p3 <- rbind(c(3,0), c(4,0), c(4,1), c(3,1), c(3,0))
  p4 <- rbind(c(3.3,0.3), c(3.8,0.3), c(3.8,0.8), c(3.3,0.8), c(3.3,0.3))[5:1,]
  p5 <- rbind(c(3,3), c(4,2), c(4,3), c(3,3))

  mpol <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_multipolygon(list(list(p1,p2), list(p3,p4), list(p5)))),
    v = "a")

  fun_geom_sf <- function(sf, show.legend) {
    p <- ggplot() + geom_sf(aes(colour = v), data = sf, show.legend = show.legend)
    ggplot_build(p)
  }

  # test the automatic choice
  expect_identical(fun_geom_sf(mp, TRUE)$plot$layers[[1]]$show.legend, TRUE)
  expect_identical(fun_geom_sf(mp, TRUE)$plot$layers[[1]]$computed_geom_params$legend, "point")

  expect_identical(fun_geom_sf(mls, TRUE)$plot$layers[[1]]$show.legend, TRUE)
  expect_identical(fun_geom_sf(mls, TRUE)$plot$layers[[1]]$computed_geom_params$legend, "line")

  expect_identical(fun_geom_sf(mpol, TRUE)$plot$layers[[1]]$show.legend, TRUE)
  expect_identical(fun_geom_sf(mpol, TRUE)$plot$layers[[1]]$computed_geom_params$legend, "polygon")

  # test that automatic choice can be overridden manually
  expect_identical(fun_geom_sf(mp, "point")$plot$layers[[1]]$show.legend, TRUE)
  expect_identical(fun_geom_sf(mp, "point")$plot$layers[[1]]$computed_geom_params$legend, "point")

  expect_identical(fun_geom_sf(mls, "point")$plot$layers[[1]]$show.legend, TRUE)
  expect_identical(fun_geom_sf(mls, "point")$plot$layers[[1]]$computed_geom_params$legend, "point")

  expect_identical(fun_geom_sf(mpol, "point")$plot$layers[[1]]$show.legend, TRUE)
  expect_identical(fun_geom_sf(mpol, "point")$plot$layers[[1]]$computed_geom_params$legend, "point")
})

test_that("geom_sf() determines the legend type from mapped geometry column", {
  skip_if_not_installed("sf")
  if (packageVersion("sf") < "0.5.3") skip("Need sf 0.5.3")

  p1 <- rbind(c(1,1), c(2,2), c(3,3))
  s1 <- rbind(c(0,3), c(0,4), c(1,5), c(2,5))
  s2 <- rbind(c(0.2,3), c(0.2,4), c(1,4.8), c(2,4.8))
  s3 <- rbind(c(0,4.4), c(0.6,5))

  d_sf <- sf::st_sf(
    g_point = sf::st_sfc(sf::st_multipoint(p1)),
    g_line = sf::st_sfc(sf::st_multilinestring(list(s1, s2, s3))),
    v = "a"
  )

  p <- ggplot_build(
    ggplot(d_sf) + geom_sf(aes(geometry = g_point, colour = "a"))
  )
  expect_identical(p$plot$layers[[1]]$computed_geom_params$legend, "point")

  p <- ggplot_build(
    ggplot(d_sf) + geom_sf(aes(geometry = g_line, colour = "a"))
  )
  expect_identical(p$plot$layers[[1]]$computed_geom_params$legend, "line")

  # If `geometry` is not a symbol, `LayerSf$setup_layer()` gives up guessing
  # the legend type, and falls back to "polygon"
  p <- ggplot_build(
    ggplot(d_sf) + geom_sf(aes(geometry = identity(g_point), colour = "a"))
  )
  expect_identical(p$plot$layers[[1]]$computed_geom_params$legend, "polygon")
})

test_that("geom_sf() removes rows containing missing aes", {
  skip_if_not_installed("sf")
  if (packageVersion("sf") < "0.5.3") skip("Need sf 0.5.3")

  grob_xy_length <- function(x) {
    g <- layer_grob(x)[[1]]
    c(length(g$x), length(g$y))
  }

  pts <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_point(0:1), sf::st_point(1:2)),
    size = c(1, NA),
    shape = c("a", NA),
    colour = c("red", NA)
  )

  p <- ggplot(pts)
  expect_warning(
    expect_identical(grob_xy_length(p + geom_sf(aes(size = size))), c(1L, 1L)),
    "Removed 1 row containing missing values or values outside the scale range"
  )
  expect_warning(
    expect_identical(grob_xy_length(p + geom_sf(aes(shape = shape))), c(1L, 1L)),
    "Removed 1 row containing missing values or values outside the scale range"
  )
  # default colour scale maps a colour even to a NA, so identity scale is needed to see if NA is removed
  expect_warning(
    expect_identical(grob_xy_length(p + geom_sf(aes(colour = colour)) + scale_colour_identity()),
                     c(1L, 1L)),
    "Removed 1 row containing missing values or values outside the scale range"
  )
})

test_that("geom_sf() handles alpha properly", {
  skip_if_not_installed("sf")
  if (packageVersion("sf") < "0.5.3") skip("Need sf 0.5.3")

  sfc <- sf::st_sfc(
    sf::st_point(0:1),
    sf::st_linestring(rbind(0:1, 1:2)),
    sf::st_polygon(list(rbind(0:1, 1:2, 2:1, 0:1)))
  )
  red <- "#FF0000FF"
  p <- ggplot(sfc) + geom_sf(colour = red, fill = red, alpha = 0.5)
  g <- layer_grob(p)[[1]]

  # alpha affects the colour of points and lines
  expect_equal(g[[1]]$gp$col, alpha(red, 0.5))
  expect_equal(g[[2]]$gp$col, alpha(red, 0.5))
  # alpha doesn't affect the colour of polygons, but the fill
  expect_equal(g[[3]]$gp$col, alpha(red, 1.0))
  expect_equal(g[[3]]$gp$fill, alpha(red, 0.5))
})

test_that("errors are correctly triggered", {
  skip_if_not_installed("sf")
  pts <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_point(0:1), sf::st_point(1:2)),
    size = c(1, NA),
    shape = c("a", NA),
    colour = c("red", NA)
  )
  p <- ggplot(pts) + geom_sf() + coord_cartesian()
  expect_snapshot_error(ggplotGrob(p))
  expect_snapshot_error(geom_sf_label(position = "jitter", nudge_x = 0.5))
  expect_snapshot_error(geom_sf_text(position = "jitter", nudge_x = 0.5))

  # #5204: missing linewidth should be dropped
  pts <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 1, 0, 1), ncol = 2)),
      sf::st_linestring(matrix(c(0, 1, 1, 0), ncol = 2))
    ),
    linewidth = c(1, NA)
  )
  expect_snapshot_warning(sf_grob(pts, na.rm = FALSE))
})

# Visual tests ------------------------------------------------------------

test_that("geom_sf draws correctly", {
  skip_if_not_installed("sf")
  if (packageVersion("sf") < "0.5.3") skip("Need sf 0.5.3")

  nc_tiny_coords <- matrix(
    c(-81.473, -81.741, -81.67, -81.345, -81.266, -81.24, -81.473,
      36.234, 36.392, 36.59, 36.573, 36.437, 36.365, 36.234),
    ncol = 2
  )

  nc <- sf::st_as_sf(
    data_frame(
      NAME = "ashe",
      geometry = sf::st_sfc(sf::st_polygon(list(nc_tiny_coords)), crs = 4326)
    )
  )


  # Perform minimal tests
  pts <- sf::st_sf(a = 1:2, geometry = sf::st_sfc(sf::st_point(0:1), sf::st_point(1:2)))
  plot <- ggplot() + geom_sf(data = pts)
  expect_error(regexp = NA, ggplot_build(plot))

  expect_doppelganger("North Carolina county boundaries",
    ggplot() + geom_sf(data = nc) + coord_sf(datum = 4326)
  )

  pts <- sf::st_sf(a = 1:2, geometry = sf::st_sfc(sf::st_point(0:1), sf::st_point(1:2)))
  expect_doppelganger("spatial points",
    ggplot() + geom_sf(data = pts)
  )
})

test_that("geom_sf uses combinations of geometry correctly", {
  skip_if_not_installed("sf")

  t <- seq(0, 2 *pi, length.out = 10)
  data <- sf::st_sf(sf::st_sfc(
    sf::st_multipoint(cbind(1:2, 3:4)),
    sf::st_multilinestring(list(
      cbind(c(1, 1.8), c(3.8, 3)),
      cbind(c(1.2, 2), c(4, 3.2))
    )),
    sf::st_polygon(list(
      cbind(cos(t), zapsmall(sin(t))),
      cbind(cos(t), zapsmall(sin(t))) + 5
    )),
    sf::st_geometrycollection(x = list(
      sf::st_point(x = c(3, 2)),
      sf::st_linestring(cbind(c(2, 4, 4), c(1, 1, 3)))
    )),
    sf::st_linestring(x = cbind(c(2, 6), c(-1, 3))),
    sf::st_point(c(5, 0))
  ))

  update_geom_defaults("point", list(colour = "blue"))
  update_geom_defaults("line", list(colour = "red"))
  # Note: polygon defaults are mostly ignored or overridden

  withr::defer({
    update_geom_defaults("point", NULL)
    update_geom_defaults("line",  NULL)
  })

  expect_doppelganger(
    "mixed geometry types",
    ggplot(data) + geom_sf()
  )
})

test_that("geom_sf_text() and geom_sf_label() draws correctly", {
  skip_if_not_installed("sf")
  if (packageVersion("sf") < "0.5.3") skip("Need sf 0.5.3")

  nc_tiny_coords <- matrix(
    c(-81.473, -81.741, -81.67, -81.345, -81.266, -81.24, -81.473,
      36.234, 36.392, 36.59, 36.573, 36.437, 36.365, 36.234),
    ncol = 2
  )

  nc <- sf::st_as_sf(
    data_frame(
      NAME = "ashe",
      geometry = sf::st_sfc(sf::st_polygon(list(nc_tiny_coords)), crs = 4326)
    )
  )

  # In order to avoid warning, transform to a projected coordinate system
  nc_3857 <- sf::st_transform(nc, 3857)

  expect_doppelganger("Texts for North Carolina",
    ggplot() + geom_sf_text(data = nc_3857, aes(label = NAME))
  )

  expect_doppelganger("Labels for North Carolina",
    ggplot() + geom_sf_label(data = nc_3857, aes(label = NAME))
  )
})

test_that("geom_sf draws arrows correctly", {
  skip_if_not_installed("sf")
  if (packageVersion("sf") < "0.5.3") skip("Need sf 0.5.3")

  nc_tiny_coords <- data_frame(
    x = c(-81.473, -81.741, -81.67, -81.345, -81.266, -81.24, -81.473),
    y = c(36.234, 36.392, 36.59, 36.573, 36.437, 36.365, 36.234)
  )

  nc <- sf::st_linestring(
      sf::st_coordinates(sf::st_as_sf(nc_tiny_coords, coords = c("x", "y"), crs = 4326))
    )

  nc2 <- sf::st_cast(
    sf::st_sfc(
      sf::st_multilinestring(lapply(
        1:(length(sf::st_coordinates(nc)[, 1]) - 1),
          function(x) rbind(
            as.numeric(sf::st_coordinates(nc)[x, 1:2]),
            as.numeric(sf::st_coordinates(nc)[x + 1, 1:2])
            )
        )
      )
    ), "LINESTRING"
  )

  expect_doppelganger("North Carolina county boundaries with arrow",
    ggplot() + geom_sf(data = nc, arrow = arrow()) + coord_sf(datum = 4326)
  )

  expect_doppelganger("North Carolina county boundaries with more than one arrow",
    ggplot() + geom_sf(data = nc2, arrow = arrow()) + coord_sf(datum = 4326)
  )
})
