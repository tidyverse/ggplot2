test_that("fill_alpha works as expected", {

  expect_snapshot_error(
    fill_alpha(data.frame(x = 1:10, y = LETTERS[1:10]), 0.5)
  )

  expect_snapshot_error(
    fill_alpha(list(list("red", "blue"), list("green", "orange")), 0.5)
  )

  # Vector input
  expect_identical(
    fill_alpha(c("red", "green"), 0.5),
    c("#FF000080", "#00FF0080")
  )

  # List input
  expect_identical(
    fill_alpha(list("red", "green"), 0.5),
    c("#FF000080", "#00FF0080")
  )

  skip_if_not_installed("grid", "4.2.0")

  # Linear gradients
  expect_identical(
    fill_alpha(list(linearGradient()), 0.5)[[1]]$colours,
    c("#00000080", "#FFFFFF80")
  )

  # Radial gradients
  expect_identical(
    fill_alpha(list(radialGradient()), 0.5)[[1]]$colours,
    c("#00000080", "#FFFFFF80")
  )

  # Tiled pattern
  pat <- pattern(
    rectGrob(c(0.25, 0.75), c(0.25, 0.75), width = 0.5, height = 0.5,
             gp = gpar(fill = "black", col = NA)),
    width = unit(1, "cm"), height = unit(1, "cm"),
    extend = "repeat"
  )
  # Constructed with empty viewport
  expect_null(environment(pat$f)$grob$vp)

  ans <- fill_alpha(list(pat), 0.5)

  # Viewport should have mask
  expect_s3_class(environment(ans[[1]]$f)$grob$vp$mask, "GridMask")
  # Should not have altered original environment
  expect_null(environment(pat$f)$grob$vp)

  # Handles plain, unlisted patterns
  expect_identical(
    fill_alpha(linearGradient(), 0.5)$colours,
    c("#00000080", "#FFFFFF80")
  )
})

test_that("geoms can use pattern fills", {

  skip_if_not_installed("grid", "4.2.0")
  skip_if_not_installed("svglite", "2.1.2")
  # TODO: ideally we should test this on all platforms, but currently they
  # don't all produce the same result
  skip_if_not(.Platform$OS.type == "windows")

  # Workaround for vdiffr's lack of pattern support
  # See also https://github.com/r-lib/vdiffr/issues/132
  custom_svg <- function(plot, file, title = "") {
    svglite::svglite(file)
    on.exit(grDevices::dev.off())
    print(
      plot + ggtitle(title) + theme_test()
    )
  }

  patterns <- list(
    linearGradient(group = FALSE),
    radialGradient(group = FALSE),
    pattern(
      rectGrob(c(0.25, 0.75), c(0.25, 0.75), width = 0.5, height = 0.5,
               gp = gpar(fill = "black", col = NA)),
      width = unit(1, "cm"), height = unit(1, "cm"),
      extend = "repeat"
    ),
    "black"
  )

  df <- data.frame(x = LETTERS[1:4], y = 2:5)

  expect_doppelganger(
    "single pattern fill",
    ggplot(df, aes(x, y)) +
      geom_col(fill = patterns[3]),
    writer = custom_svg
  )

  expect_doppelganger(
    "pattern fills, no alpha",
    ggplot(df, aes(x, y)) +
      geom_col(fill = patterns),
    writer = custom_svg
  )

  expect_doppelganger(
    "pattern fills, with alpha",
    ggplot(df, aes(x, y)) +
      geom_col(fill = patterns, alpha = c(0.8, 0.6, 0.4, 0.2)),
    writer = custom_svg
  )

  expect_doppelganger(
    "pattern fills through scale",
    ggplot(df, aes(x, y, fill = x)) +
      geom_col() +
      scale_fill_manual(values = rev(patterns)),
    writer = custom_svg
  )
})
