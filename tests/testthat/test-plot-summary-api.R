context("plot summary API")

# Note: the functions tested here are used by Shiny; please do not change
# their behavior without checking with the Shiny team first.

# Some basic plots that we build on for the tests
p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
pw <- p + facet_wrap(~ drv)
pg <- p + facet_grid(drv ~ cyl)

test_that("layout summary - basic plot", {
  l <- summarise_layout(ggplot_build(p))

  empty_named_list <- list(a=1)[0]

  expect_equal(l$panel, factor(1))
  expect_equal(l$row, 1)
  expect_equal(l$col, 1)
  expect_equal(l$vars, list(empty_named_list))
  expect_equal(l$xmin, 1.33)
  expect_equal(l$xmax, 7.27)
  expect_equal(l$ymin, 10.4)
  expect_equal(l$ymax, 45.6)
  expect_equal(l$xscale[[1]]$range$range, c(1.6, 7))
  expect_equal(l$yscale[[1]]$range$range, c(12, 44))
})

test_that("layout summary - facet_wrap", {
  lw <- summarise_layout(ggplot_build(pw))

  expect_equal(lw$panel, factor(1:3))
  expect_equal(lw$row, rep(1, 3))
  expect_equal(lw$col, 1:3)
  expect_equal(lw$vars, list(list(drv = "4"), list(drv = "f"), list(drv = "r")))
  expect_equal(lw$xmin, rep(1.33, 3))
  expect_equal(lw$xmax, rep(7.27, 3))
  expect_equal(lw$ymin, rep(10.4, 3))
  expect_equal(lw$ymax, rep(45.6, 3))
  expect_equal(lw$xscale[[1]]$range$range, c(1.6, 7))
  expect_identical(lw$xscale[[1]], lw$xscale[[2]])
  expect_identical(lw$xscale[[1]], lw$xscale[[3]])
  expect_equal(lw$yscale[[1]]$range$range, c(12, 44))
  expect_identical(lw$yscale[[1]], lw$yscale[[2]])
  expect_identical(lw$yscale[[1]], lw$yscale[[3]])
})

test_that("layout summary - facet_grid", {
  lg <- summarise_layout(ggplot_build(pg))

  expect_equal(lg$panel, factor(1:12))
  expect_equal(lg$row, rep(1:3, each = 4))
  expect_equal(lg$col, rep(1:4, 3))
  # Test just a subset of the rows, for simplicity
  expect_equal(lg$vars[[1]], list(drv = "4", cyl = 4))
  expect_equal(lg$vars[[2]], list(drv = "4", cyl = 5))
  expect_equal(lg$vars[[12]], list(drv = "r", cyl = 8))
  expect_equal(lg$xmin, rep(1.33, 12))
  expect_equal(lg$xmax, rep(7.27, 12))
  expect_equal(lg$ymin, rep(10.4, 12))
  expect_equal(lg$ymax, rep(45.6, 12))
  expect_equal(lg$xscale[[1]]$range$range, c(1.6, 7))
  expect_identical(lg$xscale[[1]], lg$xscale[[12]])
  expect_equal(lg$yscale[[1]]$range$range, c(12, 44))
  expect_identical(lg$yscale[[1]], lg$yscale[[12]])
})

test_that("layout summary - free scales", {
  pwf <- p + facet_wrap(~ drv, scales = "free")
  lwf <- summarise_layout(ggplot_build(pwf))
  expect_equal(lwf$xmin, c(1.565, 1.415, 3.640))
  expect_equal(lwf$xmax, c(6.735, 5.485, 7.160))
  expect_equal(lwf$ymin, c(11.20, 15.65, 14.45))
  expect_equal(lwf$ymax, c(28.80, 45.35, 26.55))
  expect_equal(lwf$xscale[[1]]$range$range, c(1.8, 6.5))
  expect_equal(lwf$xscale[[2]]$range$range, c(1.6, 5.3))
  expect_equal(lwf$yscale[[1]]$range$range, c(12, 28))
  expect_equal(lwf$yscale[[2]]$range$range, c(17, 44))
})

test_that("layout summary - reversed scales", {
  pr <- p + scale_x_reverse()
  lr <- summarise_layout(ggplot_build(pr))
  expect_equal(lr$xmin, -7.27)
  expect_equal(lr$xmax, -1.33)
  expect_equal(lr$xscale[[1]]$trans$name, "reverse")
  expect_equal(lr$xscale[[1]]$trans$transform(5), -5)
})

test_that("layout summary - log scales", {
  pl <- p + scale_x_log10() + scale_y_continuous(trans = "log2")
  ll <- summarise_layout(ggplot_build(pl))
  expect_equal(ll$xscale[[1]]$trans$name, "log-10")
  expect_equal(ll$xscale[[1]]$trans$transform(100), 2)
  expect_equal(ll$yscale[[1]]$trans$name, "log-2")
  expect_equal(ll$yscale[[1]]$trans$transform(16), 4)
})

test_that("coord summary - basic", {
  l <- summarise_coord(ggplot_build(p))
  expect_identical(l, list(xlog = NA_real_, ylog = NA_real_, flip = FALSE))
})

test_that("coord summary - log transformations", {
  # Check for coord log transformations (should ignore log scale)
  pl <- p + scale_x_log10() + coord_trans(x = "log2")
  ll <- summarise_coord(ggplot_build(pl))
  expect_identical(ll, list(xlog = 2, ylog = NA_real_, flip = FALSE))
})

test_that("coord summary - coord_flip", {
  pf <- p + coord_flip()
  lf <- summarise_coord(ggplot_build(pf))
  expect_identical(lf, list(xlog = NA_real_, ylog = NA_real_, flip = TRUE))
})

test_that("summarise_layers", {
  l <- summarise_layers(ggplot_build(p))
  expect_equal(l$mapping[[1]], list(x = rlang::quo(displ), y = rlang::quo(hwy)))

  p2 <- p + geom_point(aes(x = displ/2, y = hwy/2))
  l2 <- summarise_layers(ggplot_build(p2))
  expect_equal(l2$mapping[[1]], list(x = rlang::quo(displ), y = rlang::quo(hwy)))

  # Here use _identical because the quosures are supposed to be local
  expect_identical(l2$mapping[[2]], list(x = rlang::quo(displ/2), y = rlang::quo(hwy/2)))
})
