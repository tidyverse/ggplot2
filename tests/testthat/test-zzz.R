context("zzz")

test_that(".onAttach does not modify the random stream", {
  set.seed(42)
  x <- runif(5)
  set.seed(42)
  .onAttach()
  expect_equal(runif(5), x)
})
