test_that("aesthetic checking in geom throws correct errors", {
  p <- ggplot(mtcars) + geom_point(aes(disp, mpg, colour = after_scale(data)))
  expect_snapshot_error(ggplotGrob(p))

  aes <- list(a = 1:4, b = letters[1:4], c = TRUE, d = 1:2, e = 1:5)
  expect_snapshot_error(check_aesthetics(aes, 4))
})

test_that("geom defaults can be set and reset", {
  l <- geom_point()
  test <- l$geom$use_defaults(data_frame0())
  expect_equal(test$colour, "black")

  inv <- update_geom_defaults("point", list(colour = "red"))
  test <- l$geom$use_defaults(data_frame0())
  expect_equal(test$colour, "red")
  expect_equal(inv$colour, "black")

  inv <- update_geom_defaults("point", NULL)
  test <- l$geom$use_defaults(data_frame0())
  expect_equal(test$colour, "black")
  expect_equal(inv$colour, "red")
})
