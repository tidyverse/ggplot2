skip_on_cran() # This test suite is long-running (on cran) and is skipped

test_that("modifying theme element properties with + operator works", {

  # Changing a "leaf node" works
  t <- theme_grey() + theme(axis.title.x = element_text(colour = 'red', margin = margin()))
  expect_identical(t$axis.title.x, element_text(colour = 'red', margin = margin(), vjust = 1))
  # Make sure the theme class didn't change or get dropped
  expect_true(is.theme(t))
  # Make sure the element class didn't change or get dropped
  expect_true(inherits(t$axis.title.x, "element"))
  expect_true(inherits(t$axis.title.x, "element_text"))

  # Modifying an intermediate node works
  t <- theme_grey() + theme(axis.title = element_text(colour = 'red'))
  expect_identical(t$axis.title, element_text(colour = 'red'))

  # Modifying a root node changes only the specified properties
  t <- theme_grey() + theme(text = element_text(colour = 'red'))
  expect_identical(t$text$colour, 'red')
  expect_identical(t$text$family, theme_grey()$text$family)
  expect_identical(t$text$face,   theme_grey()$text$face)
  expect_identical(t$text$size,   theme_grey()$text$size)
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

  expect_error(theme_grey() + "asdf")
})

test_that("adding theme object to ggplot object with + operator works", {
  ## test with complete theme
  p <- ggplot(data.frame(x = 1:3), aes(x, x)) + geom_point() + theme_grey()
  p <- p + theme(axis.title = element_text(size = 20))
  expect_true(p$theme$axis.title$size == 20)

  # Should update specified properties, but not reset other properties
  p <- p + theme(text = element_text(colour = 'red'))
  expect_true(p$theme$text$colour == 'red')
  tt <- theme_grey()$text
  tt$colour <- 'red'
  expect_true(tt$inherit.blank)
  tt$inherit.blank <- FALSE
  expect_identical(p$theme$text, tt)

  ## test without complete theme
  p <- ggplot(data.frame(x = 1:3), aes(x, x)) + geom_point()
  p <- p + theme(axis.title = element_text(size = 20))
  expect_true(p$theme$axis.title$size == 20)

  # Should update specified properties, but not reset other properties
  p <- p + theme(text = element_text(colour = 'red'))
  expect_true(p$theme$text$colour == 'red')
  expect_null(p$theme$text$family)
  expect_null(p$theme$text$face)
  expect_null(p$theme$text$size)
  expect_null(p$theme$text$hjust)
  expect_null(p$theme$text$vjust)
  expect_null(p$theme$text$angle)
  expect_null(p$theme$text$lineheight)
  expect_null(p$theme$text$margin)
  expect_null(p$theme$text$debug)

  ## stepwise addition of partial themes is identical to one-step addition
  p <- ggplot(data.frame(x = 1:3), aes(x, x)) + geom_point()
  p1 <- p + theme_light() +
    theme(axis.line.x = element_line(color = "blue")) +
    theme(axis.ticks.x = element_line(color = "red"))

  p2 <- p + theme_light() +
    theme(axis.line.x = element_line(color = "blue"),
          axis.ticks.x = element_line(color = "red"))

  expect_identical(p1$theme, p2$theme)
})

test_that("replacing theme elements with %+replace% operator works", {
  # Changing a "leaf node" works
  t <- theme_grey() %+replace% theme(axis.title.x = element_text(colour = 'red'))
  expect_identical(t$axis.title.x, element_text(colour = 'red'))
  # Make sure the class didn't change or get dropped
  expect_true(is.theme(t))

  # Changing an intermediate node works
  t <- theme_grey() %+replace% theme(axis.title = element_text(colour = 'red'))
  expect_identical(t$axis.title, element_text(colour = 'red'))
  # Descendent is unchanged
  expect_identical(t$axis.title.x, theme_grey()$axis.title.x)

  # Adding empty theme() has no effect
  t <- theme_grey() %+replace% theme()
  expect_identical(t, theme_grey())

  expect_error(theme_grey() + "asdf")
})

test_that("calculating theme element inheritance works", {
  t <- theme_grey() + theme(axis.title = element_text(colour = 'red'))

  # Check that properties are passed along from axis.title to axis.title.x
  e <- calc_element('axis.title.x', t)
  expect_identical(e$colour, 'red')
  expect_false(is.null(e$family))
  expect_false(is.null(e$face))
  expect_false(is.null(e$size))

  # Check that rel() works for relative sizing, and is applied at each level
  t <- theme_grey(base_size = 12) +
    theme(axis.title   = element_text(size = rel(0.5))) +
    theme(axis.title.x = element_text(size = rel(0.5)))
  e <- calc_element('axis.title', t)
  expect_identical(e$size, 6)
  ex <- calc_element('axis.title.x', t)
  expect_identical(ex$size, 3)

  # Check that a theme_blank in a parent node gets passed along to children
  t <- theme_grey() + theme(text = element_blank())
  expect_identical(calc_element('axis.title.x', t), element_blank())

  # Check that inheritance from derived class works
  element_dummyrect <- function(dummy) { # like element_rect but w/ dummy argument
    structure(list(
      fill = NULL, colour = NULL, dummy = dummy, linewidth = NULL,
      linetype = NULL, inherit.blank = FALSE
    ), class = c("element_dummyrect", "element_rect", "element"))
  }

  e <- calc_element(
    "panel.background",
    theme(
      rect = element_rect(fill = "white", colour = "black", linewidth = 0.5, linetype = 1),
      panel.background = element_dummyrect(dummy = 5),
      complete = TRUE # need to prevent pulling in default theme
    )
  )

  expect_identical(
    e,
    structure(list(
      fill = "white", colour = "black", dummy = 5, linewidth = 0.5, linetype = 1,
      inherit.blank = TRUE # this is true because we're requesting a complete theme
    ), class = c("element_dummyrect", "element_rect", "element"))
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
  e2$inherit.blank <- FALSE # b/c inherit.blank = TRUE for complete themes
  expect_identical(e1, e2)

  theme <- theme_gray() +
    theme(strip.text = element_blank(), strip.text.x = element_text(inherit.blank = TRUE))
  e1 <- ggplot2:::calc_element("strip.text.x", theme)
  e2 <- ggplot2:::calc_element("strip.text", theme)
  expect_identical(e1, e2)
})

test_that("complete and non-complete themes interact correctly with each other", {
  # The 'complete' attribute of t1 + t2 is the OR of their 'complete' attributes.

  # But for _element properties_, the one on the right modifies the one on the left.
  t <- theme_bw() + theme(text = element_text(colour = 'red'))
  expect_true(attr(t, "complete"))
  expect_equal(t$text$colour, 'red')

  # A complete theme object (like theme_bw) always trumps a non-complete theme object
  t <- theme(text = element_text(colour = 'red')) + theme_bw()
  expect_true(attr(t, "complete"))
  expect_equal(t$text$colour, theme_bw()$text$colour)

  # Adding two non-complete themes: the one on the right modifies the one on the left.
  t <- theme(text = element_text(colour = 'blue')) +
    theme(text = element_text(colour = 'red'))
  expect_false(attr(t, "complete"))
  expect_equal(t$text$colour, 'red')
})

test_that("complete and non-complete themes interact correctly with ggplot objects", {
  base <- ggplot(data.frame(x = 1:3), aes(x, x)) + geom_point()

  # Check that adding two theme successive theme objects to a ggplot object
  # works like adding the two theme object to each other
  p <- ggplot_build(base + theme_bw() + theme(text = element_text(colour = 'red')))
  expect_true(attr(p$plot$theme, "complete"))

  # Compare the theme objects, after sorting the items, because item order can differ
  pt <- p$plot$theme
  tt <- theme_bw() + theme(text = element_text(colour = 'red'))
  pt <- pt[order(names(pt))]
  tt <- tt[order(names(tt))]
  expect_identical(pt, tt)

  p <- ggplot_build(base + theme(text = element_text(colour = 'red')) + theme_bw())
  expect_true(attr(p$plot$theme, "complete"))
  # Compare the theme objects, after sorting the items, because item order can differ
  pt <- p$plot$theme
  tt <- theme(text = element_text(colour = 'red')) + theme_bw()
  pt <- pt[order(names(pt))]
  tt <- tt[order(names(tt))]
  expect_identical(pt, tt)

  p <- ggplot_build(base + theme(text = element_text(colour = 'red', face = 'italic')))
  expect_false(attr(p$plot$theme, "complete"))
  expect_equal(p$plot$theme$text$colour, "red")
  expect_equal(p$plot$theme$text$face, "italic")

  p <- ggplot_build(base +
    theme(text = element_text(colour = 'red')) +
    theme(text = element_text(face = 'italic')))
  expect_false(attr(p$plot$theme, "complete"))
  expect_equal(p$plot$theme$text$colour, "red")
  expect_equal(p$plot$theme$text$face, "italic")
})

test_that("theme(validate=FALSE) means do not validate_element", {
  p <- ggplot(data.frame(x = 1:3), aes(x, x)) + geom_point()
  bw <- p + theme_bw()
  red.text <- theme(text = element_text(colour = "red"))
  bw.before <- bw + theme(animint.width = 500, validate = FALSE)
  expect_equal(bw.before$theme$animint.width, 500)

  bw.after <- p + theme(animint.width = 500, validate = FALSE) + theme_bw()
  expect_null(bw.after$theme$animint.width)

  red.after <- p + theme(animint.width = 500, validate = FALSE) + red.text
  expect_equal(red.after$theme$animint.width, 500)

  red.before <- p + red.text + theme(animint.width = 500, validate = FALSE)
  expect_equal(red.before$theme$animint.width, 500)
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
  register_theme_elements(element_tree = list(test = el_def("element_rect")))
  expect_snapshot_error(calc_element("test", theme_gray() + theme(test = element_rect())))
  expect_snapshot_error(theme_set("foo"))
})

test_that("element tree can be modified", {
  # we cannot add a new theme element without modifying the element tree
  p <- ggplot() + theme(blablabla = element_text(colour = "red"))
  expect_snapshot_error(print(p))

  register_theme_elements(
    element_tree = list(blablabla = el_def("character", "text"))
  )
  expect_snapshot_error(ggplotGrob(p))

  register_theme_elements(
    element_tree = list(blablabla = el_def("unit", "text"))
  )
  expect_snapshot_error(ggplotGrob(p))

  # things work once we add a new element to the element tree
  register_theme_elements(
    element_tree = list(blablabla = el_def("element_text", "text"))
  )
  expect_silent(ggplotGrob(p))

  p1 <- ggplot() + theme(blablabla = element_line())
  expect_snapshot_error(ggplotGrob(p1))

  # inheritance and final calculation of novel element works
  final_theme <- ggplot2:::plot_theme(p, theme_gray())
  e1 <- calc_element("blablabla", final_theme)
  e2 <- calc_element("text", final_theme)
  expect_identical(e1$family, e2$family)
  expect_identical(e1$face, e2$face)
  expect_identical(e1$size, e2$size)
  expect_identical(e1$lineheight, e2$lineheight)
  expect_identical(e1$colour, "red") # not inherited from element_text

  # existing elements can be overwritten
  ed <- el_def("element_rect", "rect")
  register_theme_elements(
    element_tree = list(axis.title = ed)
  )
  expect_identical(get_element_tree()$axis.title, ed)

  reset_theme_settings(reset_current = FALSE) # revert back to defaults
})

test_that("all elements in complete themes have inherit.blank=TRUE", {
  inherit_blanks <- function(theme) {
    all(vapply(theme, function(el) {
      if (inherits(el, "element") && !inherits(el, "element_blank")) {
        el$inherit.blank
      } else {
        TRUE
      }
    }, logical(1)))
  }
  expect_true(inherit_blanks(theme_grey()))
  expect_true(inherit_blanks(theme_bw()))
  expect_true(inherit_blanks(theme_classic()))
  expect_true(inherit_blanks(theme_dark()))
  expect_true(inherit_blanks(theme_light()))
  expect_true(inherit_blanks(theme_linedraw()))
  expect_true(inherit_blanks(theme_minimal()))
  expect_true(inherit_blanks(theme_void()))
})

test_that("elements can be merged", {
  text_base <- element_text(colour = "red", size = 10)
  expect_equal(
    merge_element(element_text(colour = "blue"), text_base),
    element_text(colour = "blue", size = 10)
  )
  rect_base <- element_rect(colour = "red", linewidth = 10)
  expect_equal(
    merge_element(element_rect(colour = "blue"), rect_base),
    element_rect(colour = "blue", linewidth = 10)
  )
  line_base <- element_line(colour = "red", linewidth = 10)
  expect_equal(
    merge_element(element_line(colour = "blue"), line_base),
    element_line(colour = "blue", linewidth = 10)
  )
  expect_error(
    merge_element(text_base, rect_base),
    "Only elements of the same class can be merged"
  )
})

test_that("theme elements that don't inherit from element can be combined", {
  expect_identical(combine_elements(1, NULL), 1)
  expect_identical(combine_elements(NULL, 1), 1)
  expect_identical(combine_elements(1, 0), 1)
})

test_that("complete plot themes shouldn't inherit from default", {
  default_theme <- theme_gray() + theme(axis.text.x = element_text(colour = "red"))
  base <- ggplot(data.frame(x = 1), aes(x, x)) + geom_point()

  ptheme <- plot_theme(base + theme(axis.text.x = element_text(colour = "blue")), default_theme)
  expect_equal(ptheme$axis.text.x$colour, "blue")

  ptheme <- plot_theme(base + theme_void(), default_theme)
  expect_null(ptheme$axis.text.x)
})

test_that("current theme can be updated with new elements", {
  old <- theme_set(theme_grey())

  b1 <- ggplot() + theme_grey()
  b2 <- ggplot()

  # works for root element
  expect_identical(
    calc_element("text", plot_theme(b1)),
    calc_element("text", plot_theme(b2))
  )

  # works for derived element
  expect_identical(
    calc_element("axis.text.x", plot_theme(b1)),
    calc_element("axis.text.x", plot_theme(b2))
  )

  # theme calculation for nonexisting element returns NULL
  expect_identical(calc_element("abcde", plot_theme(b1)), NULL)

  # element tree gets merged properly
  register_theme_elements(
    abcde = element_text(color = "blue", hjust = 0, vjust = 1),
    element_tree = list(abcde = el_def("element_text", "text"))
  )

  e1 <- calc_element("abcde", plot_theme(b2))
  e2 <- calc_element("text", plot_theme(b2))
  e2$colour <- "blue"
  e2$hjust <- 0
  e2$vjust <- 1
  expect_identical(e1, e2)

  reset_theme_settings()
  theme_set(old)
})

test_that("titleGrob() and margins() work correctly", {
  # ascenders and descenders
  g1 <- titleGrob("aaaa", 0, 0, 0.5, 0.5) # lower-case letters, no ascenders or descenders
  g2 <- titleGrob("bbbb", 0, 0, 0.5, 0.5) # lower-case letters, no descenders
  g3 <- titleGrob("gggg", 0, 0, 0.5, 0.5) # lower-case letters, no ascenders
  g4 <- titleGrob("AAAA", 0, 0, 0.5, 0.5) # upper-case letters, no descenders

  expect_equal(height_cm(g1), height_cm(g2))
  expect_equal(height_cm(g1), height_cm(g3))
  expect_equal(height_cm(g1), height_cm(g4))

  # margins
  g5 <- titleGrob("aaaa", 0, 0, 0.5, 0.5, margin = margin(t = 1, r = 0, b = 0, l = 0, unit = "cm"), margin_x = TRUE, margin_y = TRUE)
  g6 <- titleGrob("aaaa", 0, 0, 0.5, 0.5, margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "cm"), margin_x = TRUE, margin_y = TRUE)
  g7 <- titleGrob("aaaa", 0, 0, 0.5, 0.5, margin = margin(t = 0, r = 0, b = 1, l = 0, unit = "cm"), margin_x = TRUE, margin_y = TRUE)
  g8 <- titleGrob("aaaa", 0, 0, 0.5, 0.5, margin = margin(t = 0, r = 0, b = 0, l = 1, unit = "cm"), margin_x = TRUE, margin_y = TRUE)

  expect_equal(height_cm(g5), height_cm(g1) + 1)
  expect_equal(width_cm(g5), width_cm(g1))
  expect_equal(height_cm(g6), height_cm(g1))
  expect_equal(width_cm(g6), width_cm(g1) + 1)
  expect_equal(height_cm(g7), height_cm(g1) + 1)
  expect_equal(width_cm(g7), width_cm(g1))
  expect_equal(height_cm(g8), height_cm(g1))
  expect_equal(width_cm(g8), width_cm(g1) + 1)

  # no margins when set to false
  g9 <- titleGrob("aaaa", 0, 0, 0.5, 0.5, margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"), margin_x = FALSE, margin_y = TRUE)
  g10 <- titleGrob("aaaa", 0, 0, 0.5, 0.5, margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"), margin_x = TRUE, margin_y = FALSE)
  expect_equal(height_cm(g9), height_cm(g1) + 2)
  # when one of margin_x or margin_y is set to FALSE and the other to TRUE, then the dimension for FALSE turns into
  # length 1null.
  expect_equal(g9$widths, grid::unit(1, "null"))
  expect_equal(g10$heights, grid::unit(1, "null"))
  expect_equal(width_cm(g10), width_cm(g1) + 2)
})

test_that("provided themes explicitly define all elements", {
  elements <- names(.element_tree)

  t <- theme_all_null()
  expect_true(all(names(t) %in% elements))
  expect_true(all(vapply(t, is.null, logical(1))))

  t <- theme_grey()
  expect_true(all(names(t) %in% elements))

  t <- theme_bw()
  expect_true(all(names(t) %in% elements))

  t <- theme_linedraw()
  expect_true(all(names(t) %in% elements))

  t <- theme_light()
  expect_true(all(names(t) %in% elements))

  t <- theme_dark()
  expect_true(all(names(t) %in% elements))

  t <- theme_minimal()
  expect_true(all(names(t) %in% elements))

  t <- theme_classic()
  expect_true(all(names(t) %in% elements))

  t <- theme_void()
  expect_true(all(names(t) %in% elements))

  t <- theme_test()
  expect_true(all(names(t) %in% elements))
})

test_that("Theme elements are checked during build", {
  p <- ggplot(mtcars) + geom_point(aes(disp, mpg)) + theme(plot.title.position = "test")
  expect_snapshot_error(ggplotGrob(p))

  p <- ggplot(mtcars) + geom_point(aes(disp, mpg)) + theme(plot.caption.position = "test")
  expect_snapshot_error(ggplotGrob(p))

  p <- ggplot(mtcars) + geom_point(aes(disp, mpg)) + theme(plot.tag.position = "test")
  expect_snapshot_error(ggplotGrob(p))
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

test_that("themes don't change without acknowledgement", {
  df <- data_frame(x = 1:3, y = 1:3, z = c("a", "b", "a"), a = 1)
  plot <- ggplot(df, aes(x, y, colour = z)) +
    geom_point() +
    facet_wrap(~ a)

  expect_doppelganger("theme_bw", plot + theme_bw())
  expect_doppelganger("theme_classic", plot + theme_classic())
  expect_doppelganger("theme_dark", plot + theme_dark())
  expect_doppelganger("theme_minimal", plot + theme_minimal())
  expect_doppelganger("theme_gray", plot + theme_gray())
  expect_doppelganger("theme_light", plot + theme_light())
  expect_doppelganger("theme_void", plot + theme_void())
  expect_doppelganger("theme_linedraw", plot + theme_linedraw())
})

test_that("themes look decent at larger base sizes", {
  df <- data_frame(x = 1:3, y = 1:3, z = c("a", "b", "a"), a = 1)
  plot <- ggplot(df, aes(x, y, colour = z)) +
    geom_point() +
    facet_wrap(~ a)

  expect_doppelganger("theme_bw_large", plot + theme_bw(base_size = 33))
  expect_doppelganger("theme_classic_large", plot + theme_classic(base_size = 33))
  expect_doppelganger("theme_dark_large", plot + theme_dark(base_size = 33))
  expect_doppelganger("theme_minimal_large", plot + theme_minimal(base_size = 33))
  expect_doppelganger("theme_gray_large", plot + theme_gray(base_size = 33))
  expect_doppelganger("theme_light_large", plot + theme_light(base_size = 33))
  expect_doppelganger("theme_void_large", plot + theme_void(base_size = 33))
  expect_doppelganger("theme_linedraw_large", plot + theme_linedraw(base_size = 33))
})

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
      axis.ticks.length.x.top = unit(-.5, "cm"),
      axis.ticks.length.x.bottom = unit(-.25, "cm"),
      axis.ticks.length.y.left = unit(.25, "cm"),
      axis.ticks.length.y.right = unit(.5, "cm"),
      axis.text.x.bottom = element_text(margin = margin(t = .25, unit = "cm")),
      axis.text.x.top = element_text(margin = margin(b = .25, unit = "cm"))
    )
  expect_doppelganger("ticks_length", plot)
})

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

test_that("rotated axis tick labels work", {
  df <- data_frame(
    y = c(1, 2, 3),
    label = c("short", "medium size", "very long label")
  )

  plot <- ggplot(df, aes(label, y)) + geom_point() +
    theme(axis.text.x = element_text(angle = 50, hjust = 1))
  expect_doppelganger("rotated x axis tick labels", plot)
})

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

test_that("Strips can render custom elements", {
  element_test <- function(...) {
    el <- element_text(...)
    class(el) <- c('element_test', 'element_text', 'element')
    el
  }
  element_grob.element_test <- function(element, label = "", x = NULL, y = NULL, ...) {
    rectGrob(width = unit(1, "cm"), height = unit(1, "cm"))
  }
  df <- data_frame(x = 1:3, y = 1:3, a = letters[1:3])
  plot <- ggplot(df, aes(x, y)) +
    geom_point() +
    facet_wrap(~a) +
    theme(strip.text = element_test())
  expect_doppelganger("custom strip elements can render", plot)
})
