context("Boxplot")

# thanks wch for providing the test code
test_that("geom_boxplot range includes all outliers", {
  dat <- data.frame(x = 1, y = c(-(1:20) ^ 3, (1:20) ^ 3) )
  p <- ggplot_build(ggplot(dat, aes(x,y)) + geom_boxplot())

  miny <- p$panel$ranges[[1]]$y.range[1]
  maxy <- p$panel$ranges[[1]]$y.range[2]

  expect_true(miny <= min(dat$y))
  expect_true(maxy >= max(dat$y))
})

test_that("geom_boxplot for continuous x gives warning if more than one x (#992)", {
  dat <- expand.grid(x=1:2, y=c(-(1:20)^3, (1:20)^3) )
  expect_that(ggplot_build(ggplot(dat, aes(x,y)) + geom_boxplot()),
              gives_warning("Continuous x aesthetic"))

  expect_that(ggplot_build(ggplot(dat, aes(x=as.Date(x,origin=Sys.Date()),y)) + geom_boxplot()),
              gives_warning("Continuous x aesthetic"))

  expect_that(ggplot_build(ggplot(dat, aes(x,y,group=x)) + geom_boxplot()),
              not(gives_warning("Continuous x aesthetic")))

  expect_that(ggplot_build(ggplot(dat, aes(x=1,y)) + geom_boxplot()),
              not(gives_warning("Continuous x aesthetic")))

  expect_that(ggplot_build(ggplot(dat, aes(x=factor(x),y)) + geom_boxplot()),
              not(gives_warning("Continuous x aesthetic")))

  expect_that(ggplot_build(ggplot(dat, aes(x=(x == 1),y)) + geom_boxplot()),
              not(gives_warning("Continuous x aesthetic")))

  expect_that(ggplot_build(ggplot(dat, aes(x=as.character(x),y)) + geom_boxplot()),
              not(gives_warning("Continuous x aesthetic")))

  expect_that(ggplot_build(ggplot(dat, aes(x,y)) + geom_boxplot() + facet_wrap(~y)),
              gives_warning("Continuous x aesthetic"))
})
