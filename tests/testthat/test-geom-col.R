context("geom_col")

test_that("geom_col removes columns with parts outside the plot limits", {
  dat <- data.frame(x = c(1, 2, 3))

  p <- ggplot(dat, aes(x, x)) + geom_col()

  expect_warning( # warning created at render stage
    ggplotGrob(p + ylim(0.5, 4)),
    "Removed 3 rows containing missing values"
  )
  expect_warning( # warning created at build stage
    ggplot_build(p + ylim(0, 2.5)),
    "Removed 1 rows containing missing values"
  )
})
