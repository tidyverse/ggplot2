context('Empty data')

df0 <- data.frame(mpg=numeric(0), wt=numeric(0), am=numeric(0), cyl=numeric(0))

test_that("plots with empty data work", {
  # Empty data (no visible points)
  d <- pdata(ggplot(df0, aes(x=mpg,y=wt)) + geom_point())
  expect_equal(nrow(d[[1]]), 0)

  d <- pdata(ggplot() + geom_point(data=df0, aes(x=mpg,y=wt)))
  expect_equal(nrow(d[[1]]), 0)


  # Regular mtcars data, x=mpg, y=wt, normal points and points from empty data frame
  d <- pdata(ggplot(mtcars, aes(x=mpg, y=wt)) + geom_point() + geom_point(data=df0))
  expect_equal(nrow(d[[1]]), nrow(mtcars))
  expect_equal(nrow(d[[2]]), 0)

  # Regular mtcars data, but points only from empty data frame
  d <- pdata(ggplot(mtcars, aes(x=mpg, y=wt)) + geom_point(data=df0))
  expect_equal(nrow(d[[1]]), 0)
})


test_that("plots with empty data and vectors for aesthetics work", {
  # Empty data with x and y mapped to vector of values
  d <- pdata(qplot(1:5, 1:5))
  expect_equal(nrow(d[[1]]), 5)

  d <- pdata(ggplot(mapping=aes(x=1:5, y=1:5)) + geom_point())
  expect_equal(nrow(d[[1]]), 5)

  d <- pdata(ggplot() + geom_point(aes(x=1:5, y=1:5)))
  expect_equal(nrow(d[[1]]), 5)
})


test_that("plots with empty data and facets work", {
  # Empty data, facet_wrap, throws error
  expect_error(ggplot_build(ggplot(df0, aes(x=mpg, y=wt)) + geom_point() + facet_wrap(~ cyl)))

  # Empty data, facet_grid, throws error
  expect_error(ggplot_build(ggplot(df0, aes(x=x, y=y)) + geom_point() + facet_grid(am ~ cyl)))


  # points from mtcars points and points from empty data frame, facet_wrap
  d <- pdata(ggplot(mtcars, aes(x=mpg, y=wt)) + geom_point() + geom_point(data=df0) + facet_wrap(~ cyl))
  expect_equal(nrow(d[[1]]), nrow(mtcars))
  expect_equal(nrow(d[[2]]), 0)

  # points from mtcars points and points from empty data frame, facet_grid
  d <- pdata(ggplot(mtcars, aes(x=mpg, y=wt)) + geom_point() + geom_point(data=df0) + facet_grid(am ~ cyl))
  expect_equal(nrow(d[[1]]), nrow(mtcars))
  expect_equal(nrow(d[[2]]), 0)
})
