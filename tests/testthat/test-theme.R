skip_on_cran() # This test suite is long-running (on cran) and is skipped

test_that("dollar subsetting the theme does no partial matching", {
  t <- theme(foobar = 12)
  expect_null(t$foo)
  expect_equal(t$foobar, 12)
})

test_that("theme argument splicing works", {
  l <- list(a = 10, b = "c", d = c("foo", "bar"))
  test <- theme(!!!l)
  ref  <- theme(a = 10, b = "c", d = c("foo", "bar"))
  expect_equal(test, ref)
})

test_that("modifying theme element properties with + operator works", {

  # Changing a "leaf node" works
  t <- theme_grey() + theme(axis.title.x = element_text(colour = 'red', margin = margin()))
  expect_identical(t$axis.title.x, element_text(colour = 'red', margin = margin(), vjust = 1))
  # Make sure the theme class didn't change or get dropped
  expect_s7_class(t, class_theme)
  # Make sure the element class didn't change or get dropped
  expect_s7_class(t$axis.title.x, element)
  expect_s7_class(t$axis.title.x, element_text)

  # Modifying an intermediate node works
  t <- theme_grey() + theme(axis.title = element_text(colour = 'red'))
  expect_identical(t$axis.title, element_text(colour = 'red'))

  # Modifying a root node changes only the specified properties
  t <- theme_grey() + theme(text = element_text(colour = 'red'))
  expect_identical(t$text@colour, 'red')
  expect_identical(t$text@family, theme_grey()$text@family)
  expect_identical(t$text@face,   theme_grey()$text@face)
  expect_identical(t$text@size,   theme_grey()$text@size)
  # Descendent is unchanged
  expect_identical(t$axis.title.x, theme_grey()$axis.title.x)

  # Adding element_blank replaces element
  t <- theme_grey() + theme(axis.text.y = element_blank())
  expect_identical(t$axis.text.y, element_blank())

  # Adding a non-blank element to an element_blank() replaces it
  t <- t + theme(axis.text.y = element_text(colour = 'red'))
  expect_identical(t$axis.text.y, element_text(colour = 'red'))

  # Adding empty theme() has no effect
  t <- theme_grey() + theme()
  expect_identical(t, theme_grey())

  expect_snapshot(theme_grey() + "asdf", error = TRUE)
})

test_that("adding theme object to ggplot object with + operator works", {
  ## test with complete theme
  p <- ggplot(data.frame(x = 1:3), aes(x, x)) + geom_point() + theme_grey()
  p <- p + theme(axis.title = element_text(size = 20))
  expect_true(p@theme$axis.title@size == 20)

  # Should update specified properties, but not reset other properties
  p <- p + theme(text = element_text(colour = 'red'))
  expect_true(p@theme$text@colour == 'red')
  tt <- theme_grey()$text
  tt@colour <- 'red'
  expect_true(tt@inherit.blank)
  tt@inherit.blank <- FALSE
  expect_identical(p@theme$text, tt)

  ## test without complete theme
  p <- ggplot(data.frame(x = 1:3), aes(x, x)) + geom_point()
  p <- p + theme(axis.title = element_text(size = 20))
  expect_true(p@theme$axis.title@size == 20)

  # Should update specified properties, but not reset other properties
  p <- p + theme(text = element_text(colour = 'red'))
  expect_true(p@theme$text@colour == 'red')
  expect_null(p@theme$text@family)
  expect_null(p@theme$text@face)
  expect_null(p@theme$text@size)
  expect_null(p@theme$text@hjust)
  expect_null(p@theme$text@vjust)
  expect_null(p@theme$text@angle)
  expect_null(p@theme$text@lineheight)
  expect_null(p@theme$text@margin)
  expect_null(p@theme$text@debug)

  ## stepwise addition of partial themes is identical to one-step addition
  p <- ggplot(data.frame(x = 1:3), aes(x, x)) + geom_point()
  p1 <- p + theme_light() +
    theme(axis.line.x = element_line(color = "blue")) +
    theme(axis.ticks.x = element_line(color = "red"))

  p2 <- p + theme_light() +
    theme(axis.line.x = element_line(color = "blue"),
          axis.ticks.x = element_line(color = "red"))

  expect_identical(p1@theme, p2@theme)
})

test_that("calculating theme element inheritance works", {
  t <- theme_grey() + theme(axis.title = element_text(colour = 'red'))

  # Check that properties are passed along from axis.title to axis.title.x
  e <- calc_element('axis.title.x', t)
  expect_identical(e@colour, 'red')
  expect_false(is.null(e@family))
  expect_false(is.null(e@face))
  expect_false(is.null(e@size))

  # Check that rel() works for relative sizing, and is applied at each level
  t <- theme_grey(base_size = 12) +
    theme(axis.title   = element_text(size = rel(0.5))) +
    theme(axis.title.x = element_text(size = rel(0.5)))
  e <- calc_element('axis.title', t)
  expect_identical(e@size, 6)
  ex <- calc_element('axis.title.x', t)
  expect_identical(ex@size, 3)

  # Check that a theme_blank in a parent node gets passed along to children
  t <- theme_grey() + theme(text = element_blank())
  expect_identical(calc_element('axis.title.x', t), element_blank())

  # Check that inheritance from derived class works
  element_dummyrect <- S7::new_class(
    "element_dummyrect", parent = element_rect,
    properties = list(dummy = S7::class_any)
  )

  e <- calc_element(
    "panel.background",
    theme(
      rect = element_rect(fill = "white", colour = "black", linewidth = 0.5, linetype = 1, linejoin = "round"),
      panel.background = element_dummyrect(dummy = 5),
      complete = TRUE # need to prevent pulling in default theme
    )
  )

  expect_identical(
    e,
    element_dummyrect(
      fill = "white", colour = "black", dummy = 5, linewidth = 0.5, linetype = 1, linejoin = "round",
      inherit.blank = TRUE # this is true because we're requesting a complete theme
    )
  )

  # Check that blank elements are skipped in inheritance tree if and only if elements
  # don't inherit from blank.
  t <- theme_gray() +
    theme(
      strip.text = element_blank(),
      strip.text.x = element_text() # inherit.blank = FALSE is default
    )
  e1 <- calc_element("strip.text.x", t)
  e2 <- calc_element("text", t)
  e2@inherit.blank <- FALSE # b/c inherit.blank = TRUE for complete themes
  expect_identical(e1, e2)

  theme <- theme_gray() +
    theme(strip.text = element_blank(), strip.text.x = element_text(inherit.blank = TRUE))
  e1 <- ggplot2:::calc_element("strip.text.x", theme)
  e2 <- ggplot2:::calc_element("strip.text", theme)
  expect_identical(e1, e2)

  # Check that rel units are computed appropriately
  theme <- theme_gray() +
    theme(axis.ticks.length = unit(1, "cm"),
          axis.ticks.length.x = rel(0.5),
          axis.ticks.length.x.bottom = rel(4))

  expect_equal(calc_element("axis.ticks.length.y.left", theme), unit(1, "cm"))
  expect_equal(calc_element("axis.ticks.length.x.top", theme), unit(1, "cm") * 0.5)
  expect_equal(calc_element("axis.ticks.length.x.bottom", theme), unit(1, "cm") * 0.5 * 4)
})

test_that("complete and non-complete themes interact correctly with each other", {
  # The 'complete' attribute of t1 + t2 is the OR of their 'complete' attributes.

  # But for _element properties_, the one on the right modifies the one on the left.
  t <- theme_bw() + theme(text = element_text(colour = 'red'))
  expect_true(attr(t, "complete"))
  expect_equal(t$text@colour, 'red')

  # A complete theme object (like theme_bw) always trumps a non-complete theme object
  t <- theme(text = element_text(colour = 'red')) + theme_bw()
  expect_true(attr(t, "complete"))
  expect_equal(t$text@colour, theme_bw()$text@colour)

  # Adding two non-complete themes: the one on the right modifies the one on the left.
  t <- theme(text = element_text(colour = 'blue')) +
    theme(text = element_text(colour = 'red'))
  expect_false(attr(t, "complete"))
  expect_equal(t$text@colour, 'red')
})

test_that("complete and non-complete themes interact correctly with ggplot objects", {
  base <- ggplot(data.frame(x = 1:3), aes(x, x)) + geom_point()

  # Check that adding two theme successive theme objects to a ggplot object
  # works like adding the two theme object to each other
  p <- ggplot_build(base + theme_bw() + theme(text = element_text(colour = 'red')))
  expect_true(attr(p@plot@theme, "complete"))

  # Compare the theme objects, after sorting the items, because item order can differ
  pt <- p@plot@theme
  tt <- theme_bw() + theme(text = element_text(colour = 'red'))
  pt <- pt[order(names(pt))]
  tt <- tt[order(names(tt))]
  expect_identical(pt, tt)

  p <- ggplot_build(base + theme(text = element_text(colour = 'red')) + theme_bw())
  expect_true(attr(p@plot@theme, "complete"))
  # Compare the theme objects, after sorting the items, because item order can differ
  pt <- p@plot@theme
  tt <- theme(text = element_text(colour = 'red')) + theme_bw()
  pt <- pt[order(names(pt))]
  tt <- tt[order(names(tt))]
  expect_identical(pt, tt)

  p <- ggplot_build(base + theme(text = element_text(colour = 'red', face = 'italic')))
  expect_equal(p@plot@theme$text@colour, "red")
  expect_equal(p@plot@theme$text@face, "italic")

  p <- ggplot_build(base +
    theme(text = element_text(colour = 'red')) +
    theme(text = element_text(face = 'italic')))
  expect_equal(p@plot@theme$text@colour, "red")
  expect_equal(p@plot@theme$text@face, "italic")
})

test_that("theme(validate=FALSE) means do not check_element", {
  p <- ggplot(data.frame(x = 1:3), aes(x, x)) + geom_point()
  bw <- p + theme_bw()
  red.text <- theme(text = element_text(colour = "red"))
  bw.before <- bw + theme(animint.width = 500, validate = FALSE)
  expect_equal(bw.before@theme$animint.width, 500)

  bw.after <- p + theme(animint.width = 500, validate = FALSE) + theme_bw()
  expect_null(bw.after@theme$animint.width)

  red.after <- p + theme(animint.width = 500, validate = FALSE) + red.text
  expect_equal(red.after@theme$animint.width, 500)

  red.before <- p + red.text + theme(animint.width = 500, validate = FALSE)
  expect_equal(red.before@theme$animint.width, 500)
})

test_that("theme validation happens at build stage", {
  # adding a non-valid theme element to a theme is no problem
  expect_silent(theme_gray() + theme(text = 0))

  # the error occurs when we try to render the plot
  p <- ggplot() + theme(text = 0)
  expect_snapshot_error(print(p))

  # without validation, the error occurs when the element is accessed
  p <- ggplot() + theme(text = 0, validate = FALSE)
  expect_snapshot_error(print(p))
})

test_that("incorrect theme specifications throw meaningful errors", {
  expect_snapshot_error(add_theme(theme_grey(), theme(line = element_rect())))
  expect_snapshot_error(calc_element("line", theme(line = element_rect())))
  register_theme_elements(element_tree = list(test = el_def(element_rect)))
  expect_snapshot_error(calc_element("test", theme_gray() + theme(test = element_rect())))
  expect_snapshot_error(set_theme("foo"))
  reset_theme_settings()
})

test_that("Theme elements are checked during build", {
  p <- ggplot(mtcars) + geom_point(aes(disp, mpg)) + theme(plot.title.position = "test")
  expect_snapshot_error(ggplotGrob(p))

  p <- ggplot(mtcars) + geom_point(aes(disp, mpg)) + theme(plot.caption.position = "test")
  expect_snapshot_error(ggplotGrob(p))

  p <- ggplot(mtcars) + geom_point(aes(disp, mpg)) +
    theme(plot.tag.position = "test") + labs(tag = "test")
  expect_snapshot_error(ggplotGrob(p))
})

test_that("Theme validation behaves as expected", {
  tree <- get_element_tree()
  expect_silent(check_element(1,  "aspect.ratio", tree))
  expect_silent(check_element(1L, "aspect.ratio", tree))
  expect_snapshot_error(check_element("A", "aspect.ratio", tree))
})

test_that("Element subclasses are inherited", {

  # `rich` is subclass of `poor`
  poor <- element_line(colour = "red", linetype = 3)
  rich <- element_line(linetype = 2, linewidth = 2)
  class(rich) <- c("element_rich", class(rich))

  # `poor` should acquire `rich`
  test <- combine_elements(poor, rich)
  expect_s3_class(test, "element_rich")
  expect_equal(
    S7::props(test)[c("colour", "linetype", "linewidth")],
    list(colour = "red", linetype = 3, linewidth = 2)
  )

  # `rich` should stay `rich`
  test <- combine_elements(rich, poor)
  expect_s3_class(test, "element_rich")
  expect_equal(
    S7::props(test)[c("colour", "linetype", "linewidth")],
    list(colour = "red", linetype = 2, linewidth = 2)
  )

  # `sibling` is not strict subclass of `rich`
  sibling <- poor
  class(sibling) <- c("element_sibling", class(sibling))

  # `sibling` should stay `sibling`
  test <- combine_elements(sibling, rich)
  expect_s3_class(test, "element_sibling")
  expect_equal(
    S7::props(test)[c("colour", "linetype", "linewidth")],
    list(colour = "red", linetype = 3, linewidth = 2)
  )

  # `rich` should stay `rich`
  test <- combine_elements(rich, sibling)
  expect_s3_class(test, "element_rich")
  expect_equal(
    S7::props(test)[c("colour", "linetype", "linewidth")],
    list(colour = "red", linetype = 2, linewidth = 2)
  )
})

test_that("complete_theme completes a theme", {
  # `NULL` should match default
  gray <- theme_gray()
  new <- complete_theme(NULL, default = gray)
  expect_equal(S7::S7_data(new), S7::S7_data(gray))

  # Elements are propagated
  new <- complete_theme(theme(axis.line = element_line("red")), gray)
  expect_equal(new$axis.line@colour, "red")

  # Missing elements are filled in if default theme is incomplete
  new <- complete_theme(default = theme())
  expect_s3_class(new$axis.line, "ggplot2::element_blank")

  # Registered elements are included
  register_theme_elements(
    test = element_text(),
    element_tree = list(test = el_def(element_text, "text"))
  )
  new <- complete_theme(default = gray)
  expect_s3_class(new$test, "ggplot2::element_text")
  reset_theme_settings()
})

test_that("panel.widths and panel.heights works with free-space panels", {

  df <- data.frame(x = c(1, 1, 2, 1, 3), g = c("A", "B", "B", "C", "C"))

  p <- ggplotGrob(
    ggplot(df, aes(x, x)) +
      geom_point() +
      scale_x_continuous(expand = expansion(add = 1)) +
      facet_grid(~ g, scales = "free_x", space = "free_x") +
      theme(
        panel.widths = unit(11, "cm"),
        panel.spacing.x = unit(1, "cm")
      )
  )

  idx <- range(panel_cols(p)$l)
  expect_equal(as.numeric(p$widths[seq(idx[1], idx[2])]), c(2, 1, 3, 1, 4))

  p <- ggplotGrob(
    ggplot(df, aes(x, x)) +
      geom_point() +
      scale_y_continuous(expand = expansion(add = 1)) +
      facet_grid(g ~ ., scales = "free_y", space = "free_y") +
      theme(
        panel.heights = unit(11, "cm"),
        panel.spacing.y = unit(1, "cm")
      )
  )

  idx <- range(panel_rows(p)$t)
  expect_equal(as.numeric(p$heights[seq(idx[1], idx[2])]), c(2, 1, 3, 1, 4))

})

test_that("panel.withs and panel.heights preserve aspect ratios with single panels", {

  df <- data.frame(x = c(1, 2))

  p <- ggplotGrob(
    ggplot(df, aes(x, x)) +
      geom_point() +
      theme(
        aspect.ratio = 2,
        panel.heights = unit(10, "cm")
      )
  )

  width <- p$widths[panel_cols(p)$l]
  expect_equal(as.character(width), "5cm")

  p <- ggplotGrob(
    ggplot(df, aes(x, x)) +
      geom_point() +
      facet_wrap(~ x) + # This behaviour doesn't occur in multipanel plots.
      theme(
        aspect.ratio = 2,
        panel.heights = unit(10, "cm")
      )
  )

  width <- p$widths[panel_cols(p)$l]
  expect_equal(as.character(width), c("1null", "1null"))
})

test_that("theme() warns about conflicting palette options", {
  expect_silent(
    theme(palette.colour.discrete = c("dodgerblue", "orange"))
  )
  local_options(ggplot2.discrete.colour = c("red", "purple"))
  expect_snapshot_warning(
    theme(palette.colour.discrete = c("dodgerblue", "orange"))
  )
})

test_that("theme element conversion to lists works", {

  x <- element_rect(colour = "red")
  expect_type(x <- as.list(x), "list")
  expect_s7_class(convert(x, element_rect), element_rect)

  # For now, element_text doesn't round-trip.
  # Once fontwidth/fontweight/italic are implemented, it should round-trip again
  x <- as.list(element_text(colour = "red"))
  expect_snapshot_warning(
    convert(x, element_text)
  )
  x <- x[setdiff(names(x), c("fontwidth", "fontweight", "italic"))]
  expect_silent(convert(x, element_text))
})

# Visual tests ------------------------------------------------------------

test_that("aspect ratio is honored", {
  df <- cbind(data_frame(x = 1:8, y = 1:8, f = gl(2,4)), expand.grid(f1 = 1:2, f2 = 1:2, rep = 1:2))
  p <- ggplot(df, aes(x, y)) +
    geom_point() +
    theme_test() +
    labs(x = NULL, y = NULL)

  p_a <- p + theme(aspect.ratio = 3)
  p_b <- p + theme(aspect.ratio = 1 / 3)

  expect_doppelganger("height is 3 times width",
    p_a
  )
  expect_doppelganger("width is 3 times height",
    p_b
  )

  expect_doppelganger("height is 3 times width, 2 wrap facets",
    p_a + facet_wrap(~f)
  )
  expect_doppelganger("height is 3 times width, 2 column facets",
    p_a + facet_grid(.~f)
  )
  expect_doppelganger("height is 3 times width, 2 row facets",
    p_a + facet_grid(f~.)
  )
  expect_doppelganger("height is 3 times width, 2x2 facets",
    p_a + facet_grid(f1~f2)
  )
})

test_that("setting 'spacing' and 'margins' affect the whole plot", {

  df <- data_frame(x = 1:3, y = 1:3, z = c("a", "b", "a"), a = 1)
  plot <- ggplot(df, aes(x, y, colour = z)) +
    geom_point() +
    facet_wrap(~ a) +
    theme_gray()

  expect_doppelganger("large spacing", plot + theme(spacing = unit(1, "cm")))
  expect_doppelganger("large margins", plot + theme(margins = margin(1, 1, 1, 1, "cm")))

})

## Axes --------------------------------------------------------------------

test_that("axes can be styled independently", {
  plot <- ggplot() +
    geom_point(aes(1:10, 1:10)) +
    scale_x_continuous(sec.axis = dup_axis()) +
    scale_y_continuous(sec.axis = dup_axis()) +
    theme(
      axis.title.x.top = element_text(colour = 'red'),
      axis.title.x.bottom = element_text(colour = 'green'),
      axis.title.y.left = element_text(colour = 'blue'),
      axis.title.y.right = element_text(colour = 'yellow'),
      axis.text.x.top = element_text(colour = 'red'),
      axis.text.x.bottom = element_text(colour = 'green'),
      axis.text.y.left = element_text(colour = 'blue'),
      axis.text.y.right = element_text(colour = 'yellow'),
      axis.ticks.x.top = element_line(colour = 'red'),
      axis.ticks.x.bottom = element_line(colour = 'green'),
      axis.ticks.y.left = element_line(colour = 'blue'),
      axis.ticks.y.right = element_line(colour = 'yellow'),
      axis.line.x.top = element_line(colour = 'red'),
      axis.line.x.bottom = element_line(colour = 'green'),
      axis.line.y.left = element_line(colour = 'blue'),
      axis.line.y.right = element_line(colour = 'yellow')
    )
  expect_doppelganger("axes_styling", plot)
})

test_that("axes ticks can have independent lengths", {
  plot <- ggplot() +
    theme_test() +
    geom_point(aes(1:10, 1:10)) +
    scale_x_continuous(sec.axis = dup_axis()) +
    scale_y_continuous(sec.axis = dup_axis()) +
    theme(
      axis.ticks.length.x.top = unit(-0.5, "cm"),
      axis.ticks.length.x.bottom = unit(-0.25, "cm"),
      axis.ticks.length.y.left = unit(0.25, "cm"),
      axis.ticks.length.y.right = unit(0.5, "cm"),
      axis.text.x.bottom = element_text(margin = margin(t = 0.25, unit = "cm")),
      axis.text.x.top = element_text(margin = margin(b = 0.25, unit = "cm"))
    )
  expect_doppelganger("ticks_length", plot)
})

test_that("rotated axis tick labels work", {
  df <- data_frame(
    y = c(1, 2, 3),
    label = c("short", "medium size", "very long label")
  )

  plot <- ggplot(df, aes(label, y)) + geom_point() +
    theme(axis.text.x = element_text(angle = 50, hjust = 1))
  expect_doppelganger("rotated x axis tick labels", plot)
})

## Strips ------------------------------------------------------------------

test_that("strips can be styled independently", {
  df <- data_frame(x = 1:2, y = 1:2)
  plot <- ggplot(df, aes(x, y)) +
    facet_grid(x ~ y) +
    theme(
      strip.background.x = element_rect(fill = "red"),
      strip.background.y = element_rect(fill = "green")
    )
  expect_doppelganger("strip_styling", plot)
})

test_that("Strips can render custom elements", {
  element_test <- S7::new_class("element_test", element_text)
  S7::method(element_grob, element_test) <-
    function(element, label = "", x = NULL, y = NULL, ...) {
      rectGrob(width = unit(1, "cm"), height = unit(1, "cm"))
    }

  df <- data_frame(x = 1:3, y = 1:3, a = letters[1:3])
  plot <- ggplot(df, aes(x, y)) +
    geom_point() +
    facet_wrap(~a) +
    theme(strip.text = element_test())
  expect_doppelganger("custom strip elements can render", plot)
})

# Titles ------------------------------------------------------------------

test_that("plot titles and caption can be aligned to entire plot", {
  df <- data_frame(
    x = 1:3,
    y = 1:3,
    z = letters[1:3]
  )

  plot <- ggplot(df, aes(x, y, color = z)) +
    geom_point() + facet_wrap(~z) +
    labs(
      title = "Plot title aligned to entire plot",
      subtitle = "Subtitle aligned to entire plot",
      caption = "Caption aligned to panels"
    ) +
    theme(plot.title.position = "plot")
  expect_doppelganger("titles aligned to entire plot", plot)

  plot <- ggplot(df, aes(x, y, color = z)) +
    geom_point() + facet_wrap(~z) +
    labs(
      title = "Plot title aligned to panels",
      subtitle = "Subtitle aligned to panels",
      caption = "Caption aligned to entire plot"
    ) +
    theme(plot.caption.position = "plot")
  expect_doppelganger("caption aligned to entire plot", plot)

})

# Legends -----------------------------------------------------------------

test_that("Legends can on all sides of the plot with custom justification", {

  plot <- ggplot(mtcars) +
    aes(
      disp, mpg,
      colour = hp,
      fill   = factor(gear),
      shape  = factor(cyl),
      size   = drat,
      alpha = wt
    ) +
    geom_point() +
    guides(
      shape  = guide_legend(position = "top"),
      colour = guide_colourbar(position = "bottom"),
      size   = guide_legend(position = "left"),
      alpha  = guide_legend(position = "right"),
      fill   = guide_legend(position = "inside", override.aes = list(shape = 21))
    ) +
    theme_test() +
    theme(
      legend.justification.top    = "left",
      legend.justification.bottom = c(1, 0),
      legend.justification.left   = c(0, 1),
      legend.justification.right  = "bottom",
      legend.justification.inside = c(0.75, 0.75),
      legend.location = "plot"
    )

  expect_doppelganger("legends at all sides with justification", plot)
})

test_that("legend margins are correct when using relative key sizes", {

  df <- data_frame(x = 1:3, y = 1:3, a = letters[1:3])
  p <- ggplot(df, aes(x, y, colour = x, shape = a)) +
    geom_point() +
    theme_test() +
    theme(
      legend.box.background = element_rect(colour = "blue", fill = NA),
      legend.background = element_rect(colour = "red", fill = NA)
    )

  vertical <- p + guides(
    colour = guide_colourbar(theme = theme(legend.key.height = unit(1, "null"))),
    shape  = guide_legend(theme = theme(legend.key.height = unit(1/3, "null")))
  ) + theme(
    legend.box.margin = margin(t = 5, b = 10, unit = "mm"),
    legend.margin = margin(t = 10, b = 5, unit = "mm")
  )

  expect_doppelganger("stretched vertical legends", vertical)

  horizontal <- p + guides(
    colour = guide_colourbar(theme = theme(legend.key.width = unit(1, "null"))),
    shape  = guide_legend(theme = theme(legend.key.width = unit(1/3, "null")))
  ) + theme(
    legend.position = "top",
    legend.box.margin = margin(l = 5, r = 10, unit = "mm"),
    legend.margin = margin(l = 10, r = 5, unit = "mm")
  )

  expect_doppelganger("stretched horizontal legends", horizontal)
})

test_that("legends are placed correctly when using stretchy spacing", {

  df <- data.frame(x = 1:3, y = 1:3, a = letters[1:3])

  p <- ggplot(df, aes(x, y, colour = a, shape = factor(x))) +
    geom_point() +
    theme(
      legend.box.background = element_rect(colour = "blue", fill = NA),
      legend.background = element_rect(colour = "red", fill = NA)
    )

  expect_doppelganger(
    "vertical legends placed apart",
    p + theme(legend.position = "right", legend.spacing.y = unit(1, "null"))
  )

  expect_doppelganger(
    "horizontal legends placed apart",
    p + theme(legend.position = "top", legend.spacing.x = unit(1, "null"))
  )
})
