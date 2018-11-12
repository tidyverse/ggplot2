context("geom-sf")


# Visual tests ------------------------------------------------------------

test_that("geom_sf draws correctly", {
  skip_if_not_installed("sf")
  if (packageVersion("sf") < "0.5.3") skip("Need sf 0.5.3")

  f <- system.file("gpkg/nc.gpkg", package="sf")
  nc <- sf::read_sf(f)


  # Perform minimal tests as long as vdiffr tests are disabled
  plot <- ggplot() + geom_sf(data = nc)
  expect_error(regexp = NA, ggplot_build(plot))

  pts <- sf::st_sf(a = 1:2, geometry = sf::st_sfc(sf::st_point(0:1), sf::st_point(1:2)))
  plot <- ggplot() + geom_sf(data = pts)
  expect_error(regexp = NA, ggplot_build(plot))


  skip("sf tests are currently unstable")

  expect_doppelganger("North Carolina county boundaries",
    ggplot() + geom_sf(data = nc) + coord_sf(datum = 4326)
  )

  pts <- sf::st_sf(a = 1:2, geometry = sf::st_sfc(sf::st_point(0:1), sf::st_point(1:2)))
  expect_doppelganger("spatial points",
    ggplot() + geom_sf(data = pts)
  )
})

test_that("geom_sf_text() and geom_sf_label() draws correctly", {
  skip_if_not_installed("sf")
  if (packageVersion("sf") < "0.5.3") skip("Need sf 0.5.3")

  f <- system.file("gpkg/nc.gpkg", package="sf")
  nc <- sf::read_sf(f)
  # In order to avoid warning, trnasform to a projected coordinate system
  nc_3857 <- sf::st_transform(nc, "+init=epsg:3857")

  # Perform minimal tests as long as vdiffr tests are disabled
  plot <- ggplot() + geom_sf_text(data = nc_3857[1:3, ], aes(label = NAME))
  expect_error(regexp = NA, ggplot_build(plot))
  
  plot <- ggplot() + geom_sf_label(data = nc_3857[1:3, ], aes(label = NAME))
  expect_error(regexp = NA, ggplot_build(plot))
  
  skip("sf tests are currently unstable")
  
  expect_doppelganger("Texts for North Carolina",
    ggplot() + geom_sf_text(data = nc_3857[1:3, ], aes(label = NAME))
  )

  expect_doppelganger("Labels for North Carolina",
    ggplot() + geom_sf_label(data = nc_3857[1:3, ], aes(label = NAME))
  )
})
