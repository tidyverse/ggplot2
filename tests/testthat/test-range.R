test_that("continuous ranges expand as expected", {
  r <- ContinuousRange$new()

  r$train(1)
  expect_equal(r$range, c(1, 1))

  r$train(10)
  expect_equal(r$range, c(1, 10))
})

test_that("discrete ranges expand as expected", {
  r <- DiscreteRange$new()

  r$train("a")
  expect_equal(r$range, "a")

  r$train("b")
  expect_equal(r$range, c("a", "b"))

  r$train(letters)
  expect_equal(r$range, letters)
})
