test_that(".DollarNames retrieves inherited methods", {
  A <- ggproto("A", NULL, a = 1)
  B <- ggproto("B", A, b = 2)

  expect_equal(.DollarNames(B), c("b", "a"))
})

test_that("construction checks input", {
  expect_snapshot_error(ggproto("Test", NULL, function(self, a) a))
  expect_snapshot_error(ggproto("Test", NULL, a <- function(self, a) a))
  expect_snapshot_error(ggproto("Test", mtcars, a = function(self, a) a))
})
