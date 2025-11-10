skip_on_cran() # This test suite is long-running (on cran) and is skipped

test_that("guide_coloursteps and guide_bins return ordered breaks", {
  scale <- scale_colour_viridis_c(breaks = c(2, 3, 1))
  scale$train(c(0, 4))

  # Coloursteps guide is increasing order
  g <- guide_colorsteps()
  key <- g$train(scale = scale, aesthetic = "colour")$key
  expect_true(all(diff(key$.value) > 0))

  # Bins guide is increasing order
  g <- guide_bins()
  key <- g$train(scale = scale, aesthetics = "colour")$key
  expect_true(all(diff(key$.value) > 0))

  # Out of bound breaks are removed
  scale <- scale_colour_viridis_c(breaks = c(10, 20, 30, 40, 50), na.value = "grey50")
  scale$train(c(15, 45))

  g <- guide_colorsteps()
  key <- g$train(scale = scale, aesthetic = "colour")$key
  expect_equal(sum(key$colour == "grey50"), 0)
})

test_that("guide_coloursteps can parse (un)even steps from discrete scales", {

  val <- cut(1:10, breaks = c(0, 3, 5, 10), include.lowest = TRUE)
  scale <- scale_colour_viridis_d()
  scale$train(val)

  g <- guide_coloursteps(even.steps = TRUE)
  decor <- g$train(scale = scale, aesthetics = "colour")$decor
  expect_equal(decor$max - decor$min, rep(1/3, 3))

  g <- guide_coloursteps(even.steps = FALSE)
  decor <- g$train(scale = scale, aesthetics = "colour")$decor
  expect_equal(decor$max - decor$min, c(0.3, 0.2, 0.5))
})

test_that("bins can be parsed by guides for all scale types", {

  breaks <- c(90, 100, 200, 300)
  limits <- c(0, 1000)

  sc <- scale_colour_continuous(breaks = breaks)
  sc$train(limits)

  expect_equal(parse_binned_breaks(sc)$breaks, breaks)

  sc <- scale_colour_binned(breaks = breaks)
  sc$train(limits)

  expect_equal(parse_binned_breaks(sc)$breaks, breaks)

  # Note: discrete binned breaks treats outer breaks as limits
  cut <- cut(c(0, 95, 150, 250, 1000), breaks = breaks)

  sc <- scale_colour_discrete()
  sc$train(cut)

  parsed <- parse_binned_breaks(sc)
  expect_equal(
    sort(c(parsed$limits, parsed$breaks)),
    breaks
  )
})

test_that("binned breaks can have hardcoded labels when oob", {

  sc <- scale_colour_steps(breaks = 1:3, labels = as.character(1:3))
  sc$train(c(1, 2))

  g <- guide_bins()
  key <- g$train(scale = sc, aesthetic = "colour")$key
  expect_equal(key$.label, c("1", "2"))

  g <- guide_coloursteps()
  key <- g$train(scale = sc, aesthetic = "colour")$key
  expect_equal(key$.label, c("1", "2"))
})


test_that("coloursteps guide can be styled correctly", {
  df <- data_frame(x = c(1, 2, 4), y = c(6, 5, 7))

  p <- ggplot(df, aes(x, y, colour = x)) +
    geom_point() +
    scale_colour_binned(breaks = c(1.5, 2, 3))

  expect_doppelganger("guide_coloursteps looks as it should", p)
  expect_doppelganger(
    "show limits",
    p + guides(colour = guide_coloursteps(show.limits = TRUE))
  )
  expect_doppelganger(
    "bins relative to binsize",
    p + guides(colour = guide_coloursteps(even.steps = FALSE))
  )
  expect_doppelganger(
    "show ticks and transparancy",
    p +
      guides(
        colour = guide_coloursteps(
          alpha = 0.75,
          theme = theme(
            legend.ticks = element_line(linewidth = 0.5 / .pt, colour = "white")
          )
        )
      )
  )
})

test_that("binning scales understand the different combinations of limits, breaks, labels, and show.limits", {
  p <- ggplot(mpg, aes(cty, hwy, color = year)) +
    geom_point()

  expect_doppelganger(
    "coinciding limits and bins",
    p +
      scale_color_binned(
        limits = c(1999, 2008),
        breaks = c(1999, 2000, 2002, 2004, 2006)
      )
  )
  expect_doppelganger(
    "coinciding limits and bins 2",
    p +
      scale_color_binned(
        limits = c(1999, 2008),
        breaks = c(2000, 2002, 2004, 2006, 2008)
      )
  )
  expect_doppelganger(
    "coinciding limits and bins 3",
    p +
      scale_color_binned(
        limits = c(1999, 2008),
        breaks = c(1999, 2000, 2002, 2004, 2006),
        show.limits = TRUE
      )
  )
  expect_doppelganger(
    "labels when limits is in breaks",
    p +
      scale_color_binned(
        limits = c(1999, 2008),
        breaks = c(1999, 2000, 2002, 2004, 2006),
        labels = 1:5
      )
  )
  expect_snapshot_warning(ggplotGrob(
    p + scale_color_binned(labels = 1:4, show.limits = TRUE)
  ))
})
