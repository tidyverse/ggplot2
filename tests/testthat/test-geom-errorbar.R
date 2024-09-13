test_that("geom_errorbarh throws deprecation messages", {

  lifecycle::expect_deprecated(geom_errorbarh())

  p <- ggplot(
    data.frame(y = "A", min = 0, max = 10),
    aes(y = y, xmin = min, xmax = max)
  ) +
    layer(
      geom = "errorbarh",
      stat = "identity",
      position = "identity"
    )

  lifecycle::expect_deprecated(ggplot_build(p))
})
