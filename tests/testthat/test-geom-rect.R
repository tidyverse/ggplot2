test_that("geom_rect can derive corners", {

  corners <- c("xmin", "xmax", "ymin", "ymax")
  full <- data.frame(
    xmin = c(1, 2), xmax = c(3, 6),
    ymin = c(1, 2), ymax = c(3, 6),
    width = c(2, 4), height = c(2, 4),
    x = c(2, 4), y = c(2, 4)
  )

  test <- full[, c("xmin", "ymin", "width", "height")]
  test <- GeomRect$setup_data(test, NULL)
  expect_equal(full[, corners], test[, corners])

  test <- full[, c("xmin", "ymin", "x", "y")]
  test <- GeomRect$setup_data(test, NULL)
  expect_equal(full[, corners], test[, corners])

  test <- full[, c("x", "y", "width", "height")]
  test <- GeomRect$setup_data(test, NULL)
  expect_equal(full[, corners], test[, corners])

  test <- full[, c("xmax", "ymax", "width", "height")]
  test <- GeomRect$setup_data(test, NULL)
  expect_equal(full[, corners], test[, corners])

  test <- full[, c("xmax", "ymax", "x", "y")]
  test <- GeomRect$setup_data(test, NULL)
  expect_equal(full[, corners], test[, corners])

  test <- full[, c("x", "y")]
  expect_error(
    GeomRect$setup_data(test, NULL),
    "requires two of the following aesthetics"
  )
})
