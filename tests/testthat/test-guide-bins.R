skip_on_cran() # This test suite is long-running (on cran) and is skipped

test_that("bin guide can be reversed", {
  p <- ggplot(data.frame(x = c(0, 100)), aes(x, x, colour = x, fill = x)) +
    geom_point() +
    guides(
      colour = guide_bins(reverse = TRUE, show.limits = TRUE, order = 1),
      fill = guide_bins(
        reverse = TRUE,
        show.limits = FALSE,
        order = 2,
        override.aes = list(shape = 21)
      )
    )

  expect_doppelganger("reversed guide_bins", p)
})

test_that("bin guide can be styled correctly", {
  df <- data_frame(x = c(1, 2, 3), y = c(6, 5, 7))

  p <- ggplot(df, aes(x, y, size = x)) +
    geom_point() +
    scale_size_binned()

  expect_doppelganger("guide_bins looks as it should", p)
  expect_doppelganger(
    "guide_bins can show limits",
    p + guides(size = guide_bins(show.limits = TRUE))
  )
  expect_doppelganger(
    "guide_bins can show arrows",
    p +
      guides(size = guide_bins()) +
      theme_test() +
      theme(
        legend.axis.line = element_line(
          linewidth = 0.5 / .pt,
          arrow = arrow(length = unit(1.5, "mm"), ends = "both")
        )
      )
  )
  expect_doppelganger(
    "guide_bins can remove axis",
    p +
      guides(size = guide_bins()) +
      theme_test() +
      theme(
        legend.axis.line = element_blank()
      )
  )
  expect_doppelganger(
    "guide_bins work horizontally",
    p + guides(size = guide_bins(direction = "horizontal"))
  )
})
