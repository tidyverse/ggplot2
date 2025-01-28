skip_on_cran()

test_that("show.legend handles named vectors", {
  n_legends <- function(p) {
    g <- ggplotGrob(p)
    gb <- grep("guide-box", g$layout$name)
    n <- vapply(g$grobs[gb], function(x) {
      if (is.zero(x)) return(0)
      length(x$grobs) - 1
    }, numeric(1))
    sum(n)
  }

  df <- data_frame(x = 1:3, y = 20:22)
  p <- ggplot(df, aes(x = x, y = y, color = x, shape = factor(y))) +
    geom_point(size = 20)
  expect_equal(n_legends(p), 2)

  p <- ggplot(df, aes(x = x, y = y, color = x, shape = factor(y))) +
    geom_point(size = 20, show.legend = c(color = FALSE))
  expect_equal(n_legends(p), 1)

  p <- ggplot(df, aes(x = x, y = y, color = x, shape = factor(y))) +
    geom_point(size = 20, show.legend = c(color = FALSE, shape = FALSE))
  expect_equal(n_legends(p), 0)

  # c.f.https://github.com/tidyverse/ggplot2/issues/3461
  p <- ggplot(df, aes(x = x, y = y, color = x, shape = factor(y))) +
    geom_point(size = 20, show.legend = c(shape = FALSE, color = TRUE))
  expect_equal(n_legends(p), 1)
})

test_that("guide merging for guide_legend() works as expected", {

  merge_test_guides <- function(scale1, scale2) {
    scale1$guide <- guide_legend(direction = "vertical")
    scale2$guide <- guide_legend(direction = "vertical")
    scales <- scales_list()
    scales$add(scale1)
    scales$add(scale2)
    scales$set_palettes(NULL)
    scales <- scales$scales

    aesthetics <- lapply(scales, `[[`, "aesthetics")
    scales <- rep.int(scales, lengths(aesthetics))
    aesthetics <- unlist(aesthetics, FALSE, FALSE)

    guides <- guides_list(NULL)
    guides <- guides$setup(scales, aesthetics)
    guides$train(scales, labs())
    guides$merge()
    guides$params
  }

  different_limits <- merge_test_guides(
    scale_colour_hue(limits = c("a", "b", "c", "d")),
    scale_linetype_discrete(limits = c("a", "b", "c"))
  )
  expect_length(different_limits, 2)

  same_limits <- merge_test_guides(
    scale_colour_hue(limits = c("a", "b", "c")),
    scale_linetype_discrete(limits = c("a", "b", "c"))
  )
  expect_length(same_limits, 1)
  expect_equal(same_limits[[1]]$key$.label, c("a", "b", "c"))

  same_labels_different_limits <- merge_test_guides(
    scale_colour_hue(limits = c("a", "b", "c")),
    scale_linetype_discrete(limits = c("one", "two", "three"), labels = c("a", "b", "c"))
  )
  expect_length(same_labels_different_limits, 1)
  expect_equal(same_labels_different_limits[[1]]$key$.label, c("a", "b", "c"))

  same_labels_different_scale <- merge_test_guides(
    scale_colour_gradient(limits = c(0, 4), breaks = 1:3, labels = c("a", "b", "c")),
    scale_linetype_discrete(limits = c("a", "b", "c"))
  )
  expect_length(same_labels_different_scale, 1)
  expect_equal(same_labels_different_scale[[1]]$key$.label, c("a", "b", "c"))

  repeated_identical_labels <- merge_test_guides(
    scale_colour_hue(limits = c("one", "two", "three"), labels = c("label1", "label1", "label2")),
    scale_linetype_discrete(limits = c("1", "2", "3"), labels = c("label1", "label1", "label2"))
  )
  expect_length(repeated_identical_labels, 1)
  expect_equal(repeated_identical_labels[[1]]$key$.label, c("label1", "label1", "label2"))
})

test_that("size = NA doesn't throw rendering errors", {
  df <- data.frame(
    x = c(1, 2),
    group = c("a","b")
  )
  p <- ggplot(df, aes(x = x, y = 0, colour = group)) +
    geom_point(size = NA, na.rm = TRUE)

  expect_silent(plot(p))
})

test_that("legend reverse argument reverses the key", {

  scale <- scale_colour_hue()
  scale$train(LETTERS[1:4])

  guides <- guides_list(NULL)
  guides <- guides$setup(list(scale), "colour")

  guides$params[[1]]$reverse <- FALSE
  guides$train(list(scale), labels = labs())
  fwd <- guides$get_params(1)$key

  guides$params[[1]]$reverse <- TRUE
  guides$train(list(scale), labels = labs())
  rev <- guides$get_params(1)$key

  expect_equal(fwd$colour, rev(rev$colour))
})

test_that("legends can be forced to display unrelated geoms", {

  df <- data.frame(x = 1:2)

  p <- ggplot(df, aes(x, x)) +
    geom_tile(fill = "red", show.legend = TRUE) +
    scale_colour_discrete(
      limits = c("A", "B")
    )

  b <- ggplot_build(p)
  legend <- b$plot$guides$params[[1]]

  expect_equal(
    legend$decor[[1]]$data$fill,
    c("red", "red")
  )
})


# Visual tests ------------------------------------------------------------

test_that("legend directions are set correctly", {

  p <- ggplot(mtcars, aes(disp, mpg, shape = factor(cyl), colour = drat)) +
    geom_point() +
    theme_test()

  expect_doppelganger(
    "vertical legend direction",
    p + theme(legend.direction = "vertical")
  )

  expect_doppelganger(
    "horizontal legend direction",
    p + theme(legend.direction = "horizontal")
  )
})

test_that("guide_legend uses key.spacing correctly", {
  p <- ggplot(mtcars, aes(disp, mpg, colour = factor(carb))) +
    geom_point() +
    guides(colour = guide_legend(ncol = 2)) +
    theme_test() +
    theme(
      legend.key.spacing.x = unit(2, "lines"),
      legend.key.spacing.y = unit(1, "lines")
    )

  expect_doppelganger("legend with widely spaced keys", p)
})

test_that("absent titles don't take up space", {

  p <- ggplot(mtcars, aes(disp, mpg, colour = factor(cyl))) +
    geom_point() +
    theme(
      legend.title = element_blank(),
      legend.margin = margin(),
      legend.position = "top",
      legend.justification = "left",
      legend.key = element_rect(colour = "black"),
      axis.line = element_line(colour = "black")
    )

  expect_doppelganger("left aligned legend key", p)
})

test_that("size and linewidth affect key size", {
  df <- data_frame(x = c(0, 1, 2))
  p  <- ggplot(df, aes(x, x)) +
    geom_point(aes(size = x)) +
    geom_line(aes(linewidth = 2 - x)) +
    scale_size_continuous(range = c(1, 12)) +
    scale_linewidth_continuous(range = c(1, 20))

  expect_doppelganger("enlarged guides", p)
})

test_that("legend.byrow works in `guide_legend()`", {

  df <- data.frame(x = 1:6, f = LETTERS[1:6])

  p <- ggplot(df, aes(x, x, colour = f)) +
    geom_point() +
    scale_colour_discrete(
      guide = guide_legend(
        ncol = 3,
        theme = theme(legend.byrow = TRUE)
      )
    )

  expect_doppelganger("legend.byrow = TRUE", p)
})

test_that("legend.key.justification works as intended", {

  p <- ggplot(mtcars, aes(mpg, disp, colour = factor(cyl), size = drat)) +
    geom_point() +
    scale_size_continuous(
      range = c(0, 20), breaks = c(3, 4, 5), limits = c(2.5, 5)
    ) +
    scale_colour_discrete(
      labels = c("one line", "up\nto\nfour\nlines", "up\nto\nfive\nwhole\nlines")
    ) +
    theme(legend.key.justification = c(1, 0))

  expect_doppelganger("legend key justification", p)

})

