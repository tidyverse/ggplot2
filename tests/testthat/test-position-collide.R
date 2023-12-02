test_that("collide() checks the input data", {
  data <- data.frame(x = 1:4, group = 1L)

  expect_snapshot(error = TRUE, {
    collide(data, width = 1, 'test', pos_stack)
    data$y <- 1
    out <- collide(data, width = 2, 'test', pos_stack)
  })
})
