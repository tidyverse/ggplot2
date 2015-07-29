context("Facetting")

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])

test_that("facets split up the data", {
  l1 <- ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~ z)
  d1 <- pdata(l1)[[1]]

  expect_that(d1$PANEL, equals(factor(1:3)))

  l2 <- ggplot(df, aes(x, y)) + geom_point() + facet_grid(. ~ z)
  l3 <- ggplot(df, aes(x, y)) + geom_point() + facet_grid(z ~ .)

  d2 <- pdata(l2)[[1]]
  d3 <- pdata(l3)[[1]]

  expect_that(d2, equals(d3))
  expect_that(sort(names(d2)), equals(sort(c("x", "y", "group", "PANEL"))))
  expect_that(d2$PANEL, equals(factor(1:3)))
})


test_that("facets with free scales scale independently", {
  l1 <- ggplot(df, aes(x, y)) + geom_point() +
    facet_wrap(~ z, scales = "free")
  d1 <- cdata(l1)[[1]]
  expect_that(length(unique(d1$x)), equals(1))
  expect_that(length(unique(d1$y)), equals(1))

  l2 <- ggplot(df, aes(x, y)) + geom_point() +
    facet_grid(. ~ z, scales = "free")
  d2 <- cdata(l2)[[1]]
  expect_that(length(unique(d2$x)), equals(1))
  expect_that(length(unique(d2$y)), equals(3))

  l3 <- ggplot(df, aes(x, y)) + geom_point() +
    facet_grid(z ~ ., scales = "free")
  d3 <- cdata(l3)[[1]]
  expect_that(length(unique(d3$x)), equals(3))
  expect_that(length(unique(d3$y)), equals(1))
})


test_that("shrink parameter affects scaling", {
  l1 <- ggplot(df, aes(1, y)) + geom_point()
  r1 <- pranges(l1)

  expect_that(r1$x[[1]], equals(c(1, 1)))
  expect_that(r1$y[[1]], equals(c(1, 3)))

  l2 <- ggplot(df, aes(1, y)) + stat_summary(fun.y = "mean")
  r2 <- pranges(l2)
  expect_that(r2$y[[1]], equals(c(2, 2)))

  l3 <- ggplot(df, aes(1, y)) + stat_summary(fun.y = "mean") +
    facet_null(shrink = FALSE)
  r3 <- pranges(l3)
  expect_that(r3$y[[1]], equals(c(1, 3)))
})
