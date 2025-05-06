test_that("plot succeeds even if some computation fails", {
  df <- data_frame(x = 1:2, y = 1)
  p1 <- ggplot(df, aes(x, y)) + geom_point()

  b1 <- ggplot_build(p1)
  expect_length(b1$data, 1)

  p2 <- p1 + stat_summary(fun = function(x) stop("Failed computation"))

  expect_snapshot_warning(b2 <- ggplot_build(p2))
  expect_length(b2$data, 2)
})

test_that("error message is thrown when aesthetics are missing", {
  p <- ggplot(mtcars) + stat_sum()
  expect_snapshot(ggplot_build(p), error = TRUE)
})

test_that("erroneously dropped aesthetics are found and issue a warning", {

  # case 1) dropped completely

  df1 <- data_frame(
    x = c( # arbitrary random numbers
      0.42986445,  1.11153170, -1.22318013,  0.90982003,
      0.46454276, -0.42300004, -1.76139834, -0.75060412,
      0.01635474, -0.63202159
    ),
    g = rep(1:2, each = 5)
  )
  p1 <- ggplot(df1, aes(x, fill = g)) + geom_density()
  expect_snapshot_warning(ggplot_build(p1))

  # case 2-1) dropped partially

  df2 <- data_frame(
    id     = c("a", "a", "b", "b", "c"),
    colour = c(  0,   1,  10,  10,  20),   # a should be dropped
    fill   = c(  0,   0,  10,  11,  20)    # b should be dropped
  )

  p2 <- ggplot(df2, aes(id, colour = colour, fill = fill)) + geom_bar()
  expect_snapshot_warning(
    b2 <- ggplot_build(p2)
  )

  # colour is dropped because group a's colour is not constant (GeomBar$default_aes$colour is NA)
  expect_true(all(is.na(b2$data[[1]]$colour)))
  # fill is dropped because group b's fill is not constant
  expect_true(all(b2$data[[1]]$fill == "#595959FF"))

  # case 2-1) dropped partially with NA

  df3 <- data_frame(
    id     = c("a", "a", "b", "b", "c"),
    colour = c(  0,  NA,  10,  10,  20),   # a should be dropped
    fill   = c( NA,  NA,  10,  10,  20)    # a should not be dropped
  )

  p3 <- ggplot(df3, aes(id, colour = colour, fill = fill)) + geom_bar() +
    scale_fill_continuous(na.value = "#123")
  expect_snapshot_warning(b3 <- ggplot_build(p3))

  # colour is dropped because group a's colour is not constant (GeomBar$default_aes$colour is NA)
  expect_true(all(is.na(b3$data[[1]]$colour)))
  # fill is NOT dropped. Group a's fill is na.value, but others are mapped.
  expect_equal(
    b3$data[[1]]$fill == "#123",
    c(TRUE, FALSE, FALSE)
  )
})

test_that("stats can modify persistent attributes", {

  StatTest <- ggproto(
    "StatTest", Stat,
    compute_layer = function(self, data, params, layout) {
      attr(data, "foo") <- "bar"
      data
    }
  )

  p <- ggplot(mtcars, aes(disp, mpg)) +
    geom_point(stat = StatTest) +
    facet_wrap(~cyl)

  ld <- layer_data(p)
  expect_equal(attr(ld, "foo"), "bar")

})
