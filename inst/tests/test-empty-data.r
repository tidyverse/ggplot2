context('Empty data')

df0 <- data.frame(mpg=numeric(0), wt=numeric(0), am=numeric(0), cyl=numeric(0))

test_that("layers with empty data are silently omitted", {
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


test_that("layers with empty data are silently omitted with facets", {
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


test_that("data is not inherited when when data=data.frame()", {
  # Should error when totally empty data frame because there's no x and y
  expect_error(ggplot_build(ggplot(mtcars, aes(x=mpg, y=wt)) + geom_point() +
    geom_point(data=data.frame())))


  # No extra points when x and y vars exist, but are empty
  d <- pdata(ggplot(mtcars, aes(x=mpg, y=wt)) + geom_point() +
    geom_point(data = data.frame(mpg=numeric(0), wt=numeric(0))))
  expect_equal(nrow(d[[1]]), nrow(mtcars))
  expect_equal(nrow(d[[2]]), 0)

  # No extra points when x and y vars don't exist but are set
  d <- pdata(ggplot(mtcars, aes(x=mpg, y=wt)) + geom_point() +
    geom_point(data=data.frame(mpg=numeric(0), wt=numeric(0)), x = 20, y = 3, colour = "red", size = 5))
  expect_equal(nrow(d[[1]]), nrow(mtcars))
  expect_equal(nrow(d[[2]]), 0)

  # No extra points when x and y vars exist, but are empty, even when aesthetics are set
  d <- pdata(ggplot(mtcars, aes(x=mpg, y=wt)) + geom_point() +
    geom_point(data=data.frame(mpg=numeric(0), wt=numeric(0)), x = 20, y = 3, colour = "red", size = 5))
  expect_equal(nrow(d[[1]]), nrow(mtcars))
  expect_equal(nrow(d[[2]]), 0)
})


test_that("data is inherited when data=NULL", {
  # NULL should inherit data
  d <- pdata(ggplot(mtcars, aes(x=mpg, y=wt)) + geom_point() +
    geom_point(data=NULL))
  expect_equal(nrow(d[[1]]), nrow(mtcars))
  expect_equal(nrow(d[[2]]), nrow(mtcars))

  # NULL should inherit data when all aesthetics are set
  d <- pdata(ggplot(mtcars, aes(x=mpg, y=wt)) + geom_point() +
    geom_point(data=NULL, x = 20, y = 3, colour = "red", size = 5))
  expect_equal(nrow(d[[1]]), nrow(mtcars))
  expect_equal(nrow(d[[2]]), nrow(mtcars))


  # NULL should inherit data when facet_wrap is used
  d <- pdata(ggplot(mtcars, aes(x=mpg, y=wt)) + geom_point() +
    geom_point(data=NULL) +
    facet_wrap(~ cyl))
  expect_equal(nrow(d[[1]]), nrow(mtcars))
  expect_equal(nrow(d[[2]]), nrow(mtcars))

  # NULL should inherit data when all aesthetics are set and facet_wrap is used
  d <- pdata(ggplot(mtcars, aes(x=mpg, y=wt)) + geom_point() +
    geom_point(data=NULL, x = 20, y = 3, colour = "red", size = 5) +
    facet_wrap(~ cyl))
  expect_equal(nrow(d[[1]]), nrow(mtcars))
  expect_equal(nrow(d[[2]]), nrow(mtcars))
  expect_equal(sort(d[[1]]$PANEL), sort(d[[2]]$PANEL))


  # NULL should inherit data when facet_grid is used
  d <- pdata(ggplot(mtcars, aes(x=mpg, y=wt)) + geom_point() +
    geom_point(data=NULL) +
    facet_grid(am ~ cyl))
  expect_equal(nrow(d[[1]]), nrow(mtcars))
  expect_equal(nrow(d[[2]]), nrow(mtcars))

  # NULL should inherit data when all aesthetics are set and facet_grid is used
  d <- pdata(ggplot(mtcars, aes(x=mpg, y=wt)) + geom_point() +
    geom_point(data=NULL, x = 20, y = 3, colour = "red", size = 5) +
    facet_grid(am ~ cyl))
  expect_equal(nrow(d[[1]]), nrow(mtcars))
  expect_equal(nrow(d[[2]]), nrow(mtcars))
  expect_equal(sort(d[[1]]$PANEL), sort(d[[2]]$PANEL))

  # In the future, the behavior of NULL may change, and a test for waiver will
  # also be added.
})
