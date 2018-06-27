context("geom_col")

test_that("geom_col removes columns with parts outside the plot limits", {
  dat <- data.frame(x = c(1, 2, 3))

  render_plot <- function(extra = list()) {
    withr::with_pdf(NULL, print(ggplot(dat, aes(x, x)) + geom_col() + extra))
  }

  expect_warning(render_plot(ylim(0.5, 4)), "Removed 3 rows containing missing values")
  expect_warning(render_plot(ylim(0, 2.5)), "Removed 1 rows containing missing values")
})
