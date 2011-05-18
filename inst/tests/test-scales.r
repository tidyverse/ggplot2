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

test_that("identity scale preserves input values", {
  df <- data.frame(x = 1:3, z = letters[1:3])
  
  p1 <- ggplot(df, 
    aes(x, z, colour = z, fill = z, shape = z, size = x, alpha = x)) +
    geom_point() +
    scale_colour_identity() +
    scale_fill_identity() + 
    scale_shape_identity() + 
    scale_size_identity() + 
    scale_alpha_identity()
  d1 <- ggplot_build(p1)$data[[1]]

  expect_that(d1$colour, equals(as.character(df$z)))
  expect_that(d1$fill, equals(as.character(df$z)))
  expect_that(d1$shape, equals(as.character(df$z)))
  expect_that(d1$size, equals(as.numeric(df$z)))
  expect_that(d1$alpha, equals(as.numeric(df$z)))
  
  
})