context("Dotplot")

set.seed(111)
dat <- data.frame(x = LETTERS[1:2], y = rnorm(30), g = LETTERS[3:5])

test_that("Dodging works", {

  p <- ggplot(dat, aes(x = x, y = y, fill = g)) +
       geom_dotplot(binwidth=.2, binaxis="y", position="dodge", stackdir="center")

  bp <- ggplot_build(p)

  df <- bp$data[[1]]

  # Number of levels in the dodged variable
  ndodge <- 3

  # The amount of space allocated within each dodge group
  dwidth <- .9 / ndodge

  # This should be the x position for each before dodging
  xbase <- ceiling(df$group / ndodge)

  # This is the offset from dodging
  xoffset <- (df$group-1) %% ndodge - (ndodge-1) / 2
  xoffset <- xoffset * dwidth

  # Check actual x locations equal predicted x locations
  expect_true(all(abs(df$x - (xbase + xoffset)) < 1e-6))

  # Check that xmin and xmax are in the right place
  expect_true(all(abs(df$xmax - df$x - dwidth/2) < 1e-6))
  expect_true(all(abs(df$x - df$xmin - dwidth/2) < 1e-6))
})


test_that("Binning works", {

  bp <- ggplot_build(ggplot(dat, aes(x=y)) + geom_dotplot(binwidth=.4, method="histodot"))
  x <- bp$data[[1]]$x

  # Need ugly hack to make sure mod function doesn't give values like -3.99999
  # due to floating point error
  expect_true(all(abs((x - min(x) + 1e-7) %% .4) < 1e-6))


  bp <- ggplot_build(ggplot(dat, aes(x=y)) + geom_dotplot(binwidth=.4, method="dotdensity"))
  x <- bp$data[[1]]$x

  # This one doesn't ensure that dotdensity works, but it does check that it's not
  # doing fixed bin sizes
  expect_false(all(abs((x - min(x) + 1e-7) %% .4) < 1e-6))
})


test_that("NA's result in warning from stat_bindot", {
  set.seed(122)
  dat <- data.frame(x=rnorm(20))
  dat$x[c(2,10)] <- NA

  # Need to assign it to a var here so that it doesn't automatically print
  expect_that(bp <- ggplot_build(ggplot(dat, aes(x)) + geom_dotplot(binwidth=.2)),
    gives_warning("Removed 2 rows.*stat_bindot"))
})
