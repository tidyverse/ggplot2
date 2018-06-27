context("geom_bar")

test_that("geom_bar removes bars with parts outside the plot limits", {
  dat <- data.frame(x = c("a", "b", "b", "c", "c", "c"))

  render_plot <- function(extra = list()) {
    withr::with_pdf(NULL, print(ggplot(dat, aes(x)) + geom_bar() + extra))
  }

  expect_warning(render_plot(ylim(0.5, 4)), "Removed 3 rows containing missing values")
  expect_warning(render_plot(ylim(0, 2.5)), "Removed 1 rows containing missing values")
})
