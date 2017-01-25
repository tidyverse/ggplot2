context("ggproto")

test_that(".DollarNames retrieves inherited methods", {
  A <- ggproto("A", NULL, a = 1)
  B <- ggproto("B", A, b = 2)

  expect_equal(.DollarNames(B), c("b", "a"))
})
