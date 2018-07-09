context("geom_bar")

test_that("geom_bar removes bars with parts outside the plot limits", {
  dat <- data.frame(x = c("a", "b", "b", "c", "c", "c"))

  p <- ggplot(dat, aes(x)) + geom_bar()

  expect_warning( # warning created at render stage
    ggplotGrob(p + ylim(0, 2.5)),
    "Removed 1 rows containing missing values"
  )
})
