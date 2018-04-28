context("geom_boxplot")

# thanks wch for providing the test code
test_that("geom_boxplot range includes all outliers", {
  dat <- data.frame(x = 1, y = c(-(1:20) ^ 3, (1:20) ^ 3) )
  p <- ggplot_build(ggplot(dat, aes(x,y)) + geom_boxplot())

  miny <- p$layout$panel_params[[1]]$y.range[1]
  maxy <- p$layout$panel_params[[1]]$y.range[2]

  expect_true(miny <= min(dat$y))
  expect_true(maxy >= max(dat$y))
})

test_that("geom_boxplot for continuous x gives warning if more than one x (#992)", {
  dat <- expand.grid(x = 1:2, y = c(-(1:5) ^ 3, (1:5) ^ 3) )

  bplot <- function(aes = NULL, extra = list()) {
    ggplot_build(ggplot(dat, aes) + geom_boxplot(aes) + extra)
  }

  expect_warning(bplot(aes(x, y)), "Continuous x aesthetic")
  expect_warning(bplot(aes(x, y), facet_wrap(~x)), "Continuous x aesthetic")
  expect_warning(bplot(aes(Sys.Date() + x, y)), "Continuous x aesthetic")

  expect_warning(bplot(aes(x, group = x, y)), NA)
  expect_warning(bplot(aes(1, y)), NA)
  expect_warning(bplot(aes(factor(x), y)), NA)
  expect_warning(bplot(aes(x == 1, y)), NA)
  expect_warning(bplot(aes(as.character(x), y)), NA)
})

test_that("can use US spelling of colour", {
  df <- data.frame(x = 1, y = c(1:5, 100))
  plot <- ggplot(df, aes(x, y)) + geom_boxplot(outlier.color = "red")

  gpar <- layer_grob(plot)[[1]]$children[[1]]$children[[1]]$gp
  expect_equal(gpar$col, "#FF0000FF")
})

test_that("boxes with variable widths do not overlap", {
  p <- ggplot(data = iris, aes(Species, Sepal.Length)) +
    geom_boxplot(aes(colour = Sepal.Width < 3.2), varwidth = TRUE)
  d <- layer_data(p)[c("xmin", "xmax")]
  xid <- find_x_overlaps(d)
  
  expect_false(any(duplicated(xid)))
})


# Visual tests ------------------------------------------------------------

test_that("boxplot draws correctly", {
  expect_doppelganger("outlier colours",
    ggplot(mtcars, aes(x = factor(cyl), y = drat, colour = factor(cyl))) + geom_boxplot(outlier.size = 5)
  )
})
