context("Facet Strips")

strip_layout <- function(p) {
  data <- ggplot_build(p)
  plot <- data$plot
  layout <- data$layout
  data <- data$data
  theme <- plot_theme(plot)

  geom_grobs <- Map(function(l, d) l$draw_geom(d, layout), plot$layers, data)

  facet <- layout$render(geom_grobs, data, theme, plot$labels)
  layout <- facet$layout
  strip_layout <- layout[grepl("^strip", layout$name), 1:4]
  as.list(strip_layout)
}

p <- ggplot(mtcars, aes(disp, drat)) + geom_point()


test_that("facet_wrap() builds correct output", {
  wrap <- p + facet_wrap(~cyl)

  wrap_expected <- list(
    t = c(3, 3, 3),
    l = c(3, 7, 11),
    b = c(3, 3, 3),
    r = c(3, 7, 11)
  )

  expect_equal(strip_layout(wrap), wrap_expected)
})

test_that("facet_wrap() switches to 'bottom'", {
  wrap_b <- p + facet_wrap(~cyl, strip.position = "bottom")

  wrap_b_expected <- list(
    t = c(4, 4, 4),
    l = c(3, 7, 11),
    b = c(4, 4, 4),
    r = c(3, 7, 11)
  )

  expect_equal(strip_layout(wrap_b), wrap_b_expected)
})

test_that("facet_wrap() switches to 'left'", {
  wrap_l <- p + facet_wrap(~cyl, strip.position = "left")

  wrap_l_expected <- list(
    t = c(3, 3, 3),
    l = c(13, 8, 3),
    b = c(3, 3, 3),
    r = c(13, 8, 3)
  )

  expect_equal(strip_layout(wrap_l), wrap_l_expected)
})

test_that("facet_wrap() switches to 'right'", {
  wrap_r <- p + facet_wrap(~cyl, strip.position = "right")

  wrap_r_expected <- list(
    t = c(3, 3, 3),
    l = c(14, 9, 4),
    b = c(3, 3, 3),
    r = c(14, 9, 4)
  )

  expect_equal(strip_layout(wrap_r), wrap_r_expected)
})

test_that("facet_grid() builds correct output", {
  grid <- p + facet_grid(~cyl)

  grid_expected <- list(
    t = c(3, 3, 3),
    l = c(3, 5, 7),
    b = c(3, 3, 3),
    r = c(3, 5, 7)
  )

  expect_equal(strip_layout(grid), grid_expected)
})

test_that("facet_grid() switches to 'x'", {
  grid_x <- p + facet_grid(am ~ cyl, switch = "x")

  grid_x_expected <- list(
    t = c(6, 6, 6, 3, 5),
    l = c(3, 5, 7, 8, 8),
    b = c(6, 6, 6, 3, 5),
    r = c(3, 5, 7, 8, 8)
  )

  expect_equal(strip_layout(grid_x), grid_x_expected)
})

test_that("facet_grid() switches to 'y'", {
  grid_y <- p + facet_grid(am ~ cyl, switch = "y")

  grid_y_expected <- list(
    t = c(3, 3, 3, 4, 6),
    l = c(4, 6, 8, 3, 3),
    b = c(3, 3, 3, 4, 6),
    r = c(4, 6, 8, 3, 3)
  )

  expect_equal(strip_layout(grid_y), grid_y_expected)
})

test_that("facet_grid() switches to both 'x' and 'y'", {
  grid_xy <- p + facet_grid(am ~ cyl, switch = "both")

  grid_xy_expected <- list(
    t = c(6, 6, 6, 3, 5),
    l = c(4, 6, 8, 3, 3),
    b = c(6, 6, 6, 3, 5),
    r = c(4, 6, 8, 3, 3)
  )

  expect_equal(strip_layout(grid_xy), grid_xy_expected)
})

test_that("strips can be removed", {
  dat <- data.frame(a = rep(LETTERS[1:10], 10), x = rnorm(100), y = rnorm(100))
  g <- ggplot(dat, aes(x = x, y = y)) +
    geom_point() +
    facet_wrap(~a) +
    theme(strip.background = element_blank(), strip.text = element_blank())
  g_grobs <- ggplotGrob(g)
  strip_grobs <- g_grobs$grobs[grepl('strip-', g_grobs$layout$name)]
  expect_true(all(sapply(strip_grobs, inherits, 'zeroGrob')))
})

test_that("y strip labels are rotated when strips are switched", {
  switched <- p + facet_grid(am ~ cyl, switch = "both")
  
  expect_doppelganger("switched facet strips", switched)
})
