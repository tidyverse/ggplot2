context("Scales")

test_that("ranges update only for variables listed in aesthetics", {
  sc <- scale_alpha()

  scale_train_df(sc, data.frame(alpha = 1:10))
  expect_equal(sc$range$range, c(1, 10))
  
  scale_train_df(sc, data.frame(alpha = 50))
  expect_equal(sc$range$range, c(1, 50))
  
  scale_train_df(sc, data.frame(beta = 100))
  expect_equal(sc$range$range, c(1, 50))
  
  scale_train_df(sc, data.frame())
  expect_equal(sc$range$range, c(1, 50))
  
})

test_that("mapping works", {
  sc <- scale_alpha(range = c(0, 1), na.value = 0)
  scale_train_df(sc, data.frame(alpha = 1:10))
  
  expect_equal(
    scale_map_df(sc, data.frame(alpha = 1:10))[[1]], 
    seq(0, 1, length = 10))
    
  expect_equal(scale_map_df(sc, data.frame(alpha = NA))[[1]], 0)
  
  expect_equal(
    scale_map_df(sc, data.frame(alpha = c(-10, 11)))[[1]],
    c(0, 0))
})