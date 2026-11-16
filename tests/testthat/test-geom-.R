test_that("aesthetic checking in geom throws correct errors", {
  p <- ggplot(mtcars) + geom_point(aes(disp, mpg, colour = after_scale(data)))
  expect_snapshot_error(ggplotGrob(p))

  aes <- list(a = 1:4, b = letters[1:4], c = TRUE, d = 1:2, e = 1:5)
  expect_snapshot_error(check_aesthetics(aes, 4))
})
