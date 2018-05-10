context("geom_smooth")

test_that("Data is ordered by x", {
  df <- data.frame(x = c(1, 5, 2, 3, 4), y = 1:5)

  ps <- ggplot(df, aes(x, y))+
    geom_smooth(stat = "identity", se = FALSE)

  expect_equal(layer_data(ps)[c("x", "y")], df[order(df$x), ])
})

# Visual tests ------------------------------------------------------------

test_that("geom_smooth() works with alternative stats", {
  df <- data.frame(x = c(1, 1, 2, 2, 1, 1, 2, 2),
                   y = c(1, 2, 2, 3, 2, 3, 1, 2),
                   fill = c(rep("A", 4), rep("B", 4)))

  expect_doppelganger("ribbon turned on in geom_smooth", {
    ggplot(df, aes(x, y, color = fill, fill = fill)) +
      geom_smooth(stat = "summary") # ribbon on by default
  })

  expect_doppelganger("ribbon turned off in geom_smooth", {
    ggplot(df, aes(x, y, color = fill, fill = fill)) +
      geom_smooth(stat = "summary", se = FALSE) # ribbon is turned off via `se = FALSE`
  })
})
