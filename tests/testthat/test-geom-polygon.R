context("geom-polygon")


# Visual tests ------------------------------------------------------------

test_that("geom_polygon draws correctly", {
  vdiffr::expect_doppelganger("stat_density2d with paths",
    ggplot(faithful, aes(x = eruptions, y = waiting)) +
      stat_density_2d(aes(colour = ..level..), geom = "path") +
      xlim(0.5, 6) + ylim(40, 110)
  )
  vdiffr::expect_doppelganger("stat_density2d with filled polygons",
    ggplot(faithful, aes(x = eruptions, y = waiting)) +
      stat_density2d(aes(fill = ..level..), geom = "polygon", colour = "white") +
      xlim(0.5, 6) + ylim(40, 110)
  )
})
