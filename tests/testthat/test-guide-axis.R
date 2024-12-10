skip_on_cran() # This test suite is long-running (on cran) and is skipped

test_that("axis_label_overlap_priority always returns the correct number of elements", {
  expect_identical(axis_label_priority(0), numeric(0))
  expect_setequal(axis_label_priority(1), seq_len(1))
  expect_setequal(axis_label_priority(5), seq_len(5))
  expect_setequal(axis_label_priority(10), seq_len(10))
  expect_setequal(axis_label_priority(100), seq_len(100))
})

test_that("a warning is generated when guides are drawn at a location that doesn't make sense", {
  plot <- ggplot(mpg, aes(class, hwy)) +
    geom_point() +
    scale_y_continuous(guide = guide_axis(position = "top"))
  expect_snapshot_warning(ggplot_build(plot))
})

test_that("a warning is not generated when a guide is specified with duplicate breaks", {
  plot <- ggplot(mpg, aes(class, hwy)) +
    geom_point() +
    scale_y_continuous(breaks = c(20, 20))
  built <- expect_silent(ggplot_build(plot))
  expect_silent(ggplot_gtable(built))
})

test_that("a warning is generated when more than one position guide is drawn at a location", {
  plot <- ggplot(mpg, aes(class, hwy)) +
    geom_point() +
    guides(
      y = guide_axis(position = "left"),
      y.sec = guide_axis(position = "left")
    )
  built <- expect_silent(ggplot_build(plot))

  expect_snapshot_warning(ggplot_gtable(built))
})

test_that("a warning is not generated when properly changing the position of a guide_axis()", {
  plot <- ggplot(mpg, aes(class, hwy)) +
    geom_point() +
    guides(
      y = guide_axis(position = "right")
    )
  built <- expect_silent(ggplot_build(plot))
  expect_silent(ggplot_gtable(built))
})

test_that("Using non-position guides for position scales results in an informative error", {
  p <- ggplot(mpg, aes(cty, hwy)) +
    geom_point() +
    scale_x_continuous(guide = guide_legend())
  expect_snapshot_warning(ggplot_build(p))
})

test_that("guide_axis_logticks calculates appropriate ticks", {

  test_scale <- function(transform = transform_identity(), limits = c(NA, NA)) {
    scale <- scale_x_continuous(transform = transform)
    scale$train(scale$transform(limits))
    view_scale_primary(scale)
  }

  train_guide <- function(guide, scale) {
    params <- guide$params
    params$position <- "bottom"
    guide$train(params, scale, "x")
  }

  guide <- guide_axis_logticks(negative.small = 10)
  outcome <- c((1:10)*10, (2:10)*100)

  # Test the classic log10 transformation
  scale <- test_scale(transform_log10(), c(10, 1000))
  key <- train_guide(guide, scale)$logkey

  expect_equal(sort(key$x), log10(outcome))
  expect_equal(key$.type, rep(c(1,2,3), c(3, 2, 14)))

  # Test compound transformation
  scale <- test_scale(transform_compose(transform_log10(), transform_reverse()), c(10, 1000))
  key   <- train_guide(guide, scale)$logkey

  expect_equal(sort(key$x), -log10(rev(outcome)))

  # Test transformation with negatives
  scale <- test_scale(transform_pseudo_log(), c(-1000, 1000))
  key   <- train_guide(guide, scale)$logkey

  unlog <- sort(transform_pseudo_log()$inverse(key$x))
  expect_equal(unlog, c(-rev(outcome), 0, outcome))
  expect_equal(key$.type, rep(c(1,2,3), c(7, 4, 28)))

  # Test expanded argument
  scale <- test_scale(transform_log10(), c(20, 900))
  scale$continuous_range <- c(1, 3)

  guide <- guide_axis_logticks(expanded = TRUE)
  key   <- train_guide(guide, scale)$logkey

  expect_equal(sort(key$x), log10(outcome))

  guide <- guide_axis_logticks(expanded = FALSE)
  key   <- train_guide(guide, scale)$logkey

  expect_equal(sort(key$x), log10(outcome[-c(1, length(outcome))]))

  # Test with prescaled input
  guide <- guide_axis_logticks(prescale.base = 2)
  scale <- test_scale(limits = log2(c(10, 1000)))

  key <- train_guide(guide, scale)$logkey
  expect_equal(sort(key$x), log2(outcome))

  # Should warn when scale also has transformation
  scale <- test_scale(transform_log10(), limits = c(10, 1000))
  expect_snapshot_warning(train_guide(guide, scale)$logkey)
})

# Visual tests ------------------------------------------------------------

test_that("axis guides are drawn correctly", {
  theme_test_axis <- theme_test() + theme(axis.line = element_line(linewidth = 0.5))
  test_draw_axis <- function(n_breaks = 3,
                             break_positions = seq_len(n_breaks) / (n_breaks + 1),
                             labels = as.character,
                             positions = c("top", "right", "bottom", "left"),
                             theme = theme_test_axis,
                             ...) {

    break_labels <- labels(seq_along(break_positions))

    # create the axes
    axes <- lapply(positions, function(position) {
      draw_axis(break_positions, break_labels, axis_position = position, theme = theme, ...)
    })
    axes_grob <- gTree(children = do.call(gList, axes))

    # arrange them so there's some padding on each side
    gt <- gtable(
      widths = unit(c(0.05, 0.9, 0.05), "npc"),
      heights = unit(c(0.05, 0.9, 0.05), "npc")
    )
    gt <- gtable_add_grob(gt, list(axes_grob), 2, 2, clip = "off")
    plot(gt)
  }

  # basic
  expect_doppelganger("axis guides basic", function() test_draw_axis())
  expect_doppelganger("axis guides, zero breaks", function() test_draw_axis(n_breaks = 0))

  # overlapping text
  expect_doppelganger(
    "axis guides, check overlap",
    function() test_draw_axis(20, labels = function(b) comma(b * 1e9), check.overlap = TRUE)
  )

  # rotated text
  expect_doppelganger(
    "axis guides, zero rotation",
    function() test_draw_axis(10, labels = function(b) comma(b * 1e3), angle = 0)
  )

  expect_doppelganger(
    "axis guides, positive rotation",
    function() test_draw_axis(10, labels = function(b) comma(b * 1e3), angle = 45)
  )

  expect_doppelganger(
    "axis guides, negative rotation",
    function() test_draw_axis(10, labels = function(b) comma(b * 1e3), angle = -45)
  )

  expect_doppelganger(
    "axis guides, vertical rotation",
    function() test_draw_axis(10, labels = function(b) comma(b * 1e3), angle = 90)
  )

  expect_doppelganger(
    "axis guides, vertical negative rotation",
    function() test_draw_axis(10, labels = function(b) comma(b * 1e3), angle = -90)
  )

  # dodged text
  expect_doppelganger(
    "axis guides, text dodged into rows/cols",
    function() test_draw_axis(10, labels = function(b) comma(b * 1e9), n.dodge = 2)
  )
})

test_that("axis guides are drawn correctly in plots", {
  expect_doppelganger("align facet labels, facets horizontal",
                      ggplot(mpg, aes(hwy, reorder(model, hwy))) +
                        geom_point() +
                        facet_grid(manufacturer ~ ., scales = "free", space = "free") +
                        theme_test() +
                        theme(strip.text.y = element_text(angle = 0))
  )
  expect_doppelganger("align facet labels, facets vertical",
                      ggplot(mpg, aes(reorder(model, hwy), hwy)) +
                        geom_point() +
                        facet_grid(. ~ manufacturer, scales = "free", space = "free") +
                        theme_test() +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  )
  expect_doppelganger("thick axis lines",
                      ggplot(mtcars, aes(wt, mpg)) +
                        geom_point() +
                        theme_test() +
                        theme(axis.line = element_line(linewidth = 5, lineend = "square"))
  )
})

test_that("axis guides can be customized", {
  plot <- ggplot(mpg, aes(class, hwy)) +
    geom_point() +
    scale_y_continuous(
      sec.axis = dup_axis(guide = guide_axis(n.dodge = 2)),
      guide = guide_axis(n.dodge = 2)
    ) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))

  expect_doppelganger("guide_axis() customization", plot)
})

test_that("guides can be specified in guides()", {
  plot <- ggplot(mpg, aes(class, hwy)) +
    geom_point() +
    guides(
      x = guide_axis(n.dodge = 2),
      y = guide_axis(n.dodge = 2),
      x.sec = guide_axis(n.dodge = 2),
      y.sec = guide_axis(n.dodge = 2)
    )

  expect_doppelganger("guides specified in guides()", plot)
})

test_that("guides have the final say in x and y", {
  df <- data_frame(x = 1, y = 1)
  plot <- ggplot(df, aes(x, y)) +
    geom_point() +
    guides(
      x = guide_none(title = "x (primary)"),
      y = guide_none(title = "y (primary)"),
      x.sec = guide_none(title = "x (secondary)"),
      y.sec = guide_none(title = "y (secondary)")
    )

  expect_doppelganger("position guide titles", plot)
})

test_that("Axis titles won't be blown away by coord_*()", {
  df <- data_frame(x = 1, y = 1)
  plot <- ggplot(df, aes(x, y)) +
    geom_point() +
    guides(
      x = guide_axis(title = "x (primary)"),
      y = guide_axis(title = "y (primary)"),
      x.sec = guide_axis(title = "x (secondary)"),
      y.sec = guide_axis(title = "y (secondary)")
    )

  expect_doppelganger("guide titles with coord_trans()", plot + coord_trans())
  # TODO
  # expect_doppelganger("guide titles with coord_polar()", plot + coord_polar())
  # TODO
  # expect_doppelganger("guide titles with coord_sf()", plot + coord_sf())
})

test_that("guide_axis() draws minor ticks correctly", {
  p <- ggplot(mtcars, aes(wt, disp)) +
    geom_point() +
    theme(axis.ticks.length = unit(1, "cm"),
          axis.ticks.x.bottom = element_line(linetype = 2),
          axis.ticks.length.x.top = unit(-0.5, "cm"),
          axis.minor.ticks.x.bottom = element_line(colour = "red"),
          axis.minor.ticks.length.y.left = unit(-0.5, "cm"),
          axis.minor.ticks.length.x.top = unit(-0.5, "cm"),
          axis.minor.ticks.length.x.bottom = unit(0.75, "cm"),
          axis.minor.ticks.length.y.right = unit(5, "cm")) +
    scale_x_continuous(labels = label_math()) +
    guides(
      # Test for styling and style inheritance
      x = guide_axis(minor.ticks = TRUE),
      # # Test for opposed lengths
      y = guide_axis(minor.ticks = TRUE),
      # # Test for flipped lenghts
      x.sec = guide_axis(minor.ticks = TRUE),
      # # Test that minor.length doesn't influence spacing when no minor ticks are drawn
      y.sec = guide_axis(minor.ticks = FALSE)
    )
  expect_doppelganger("guides with minor ticks", p)
})

test_that("axis guides can be capped", {
  p <- ggplot(mtcars, aes(hp, disp)) +
    geom_point() +
    theme(axis.line = element_line()) +
    guides(
      x = guide_axis(cap = "both"),
      y = guide_axis(cap = "upper"),
      y.sec = guide_axis(cap = "lower"),
      x.sec = guide_axis(cap = "none")
    )
  expect_doppelganger("axis guides with capped ends", p)
})

test_that("guide_axis_stack stacks axes", {

  left   <- guide_axis_stack("axis", guide_axis(cap = "both"), title = "left")
  right  <- guide_axis_stack("axis", guide_axis(cap = "both"), title = "right")
  bottom <- guide_axis_stack("axis", guide_axis(cap = "both"), title = "bottom")
  top    <- guide_axis_stack("axis", guide_axis(cap = "both"), title = "top")

  p <- ggplot(mtcars, aes(hp, disp)) +
    geom_point() +
    theme(axis.line = element_line()) +
    guides(x = bottom, x.sec = top, y = left, y.sec = right)
  expect_doppelganger("stacked axes", p)

  bottom <- guide_axis_stack("axis_theta", guide_axis_theta(cap = "both"))
  top    <- guide_axis_stack("axis_theta", guide_axis_theta(cap = "both"))

  p <- ggplot(mtcars, aes(hp, disp)) +
    geom_point() +
    theme(axis.line = element_line()) +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, inner.radius = 0.5) +
    guides(theta = top, theta.sec = bottom, r = left, r.sec = right)
  expect_doppelganger("stacked radial axes", p)

})

test_that("logticks look as they should", {

  p <- ggplot(data.frame(x = c(-100, 100), y = c(10, 1000)), aes(x, y)) +
    geom_point() +
    scale_y_continuous(
      transform = transform_compose(transform_log10(), transform_reverse()),
      expand = expansion(add = 0.5)
    ) +
    scale_x_continuous(
      breaks = c(-100, -10, -1, 0, 1, 10, 100)
    ) +
    coord_trans(x = transform_pseudo_log()) +
    theme_test() +
    theme(axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          axis.ticks.length.x.top = unit(-2.75, "pt")) +
    guides(
      x = guide_axis_logticks(
        title = "Pseudo-logticks with 1 as smallest tick",
        negative.small = 1
      ),
      y = guide_axis_logticks(
        title = "Inverted logticks with swapped tick lengths",
        long = 0.75, short = 2.25
      ),
      x.sec = guide_axis_logticks(
        negative.small = 0.1,
        title = "Negative length pseudo-logticks with 0.1 as smallest tick"
      ),
      y.sec = guide_axis_logticks(
        expanded = FALSE, cap = "both",
        title = "Capped and not-expanded inverted logticks"
      )
    )
  expect_doppelganger("logtick axes with customisation", p)
})

test_that("guide_axis_theta sets relative angle", {

  p <- ggplot(mtcars, aes(disp, mpg)) +
    geom_point() +
    scale_x_continuous(breaks = breaks_width(25)) +
    coord_radial(inner.radius = 0.5) +
    guides(
      theta = guide_axis_theta(angle = 0, cap = "none"),
      theta.sec = guide_axis_theta(angle = 90, cap = "both")
    ) +
    theme(axis.line = element_line(colour = "black"))

  expect_doppelganger("guide_axis_theta with angle adapting to theta", p)
})

test_that("guide_axis_theta can be used in cartesian coordinates", {

  p <- ggplot(mtcars, aes(disp, mpg)) +
    geom_point() +
    guides(x = "axis_theta", y = "axis_theta",
           x.sec = "axis_theta", y.sec = "axis_theta") +
    theme(
      axis.line.x.bottom = element_line(colour = "tomato"),
      axis.line.x.top    = element_line(colour = "limegreen"),
      axis.line.y.left   = element_line(colour = "dodgerblue"),
      axis.line.y.right  = element_line(colour = "orchid")
    )

  expect_doppelganger("guide_axis_theta in cartesian coordinates", p)
})
