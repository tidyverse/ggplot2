context("Themes")

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

  p <- qplot(1:3, 1:3)
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
  # Check that adding two theme successive theme objects to a ggplot object
  # works like adding the two theme object to each other
  p <- ggplot_build(qplot(1:3, 1:3) + theme_bw() + theme(text = element_text(colour = 'red')))
  expect_true(attr(p$plot$theme, "complete"))

  # Compare the theme objects, after sorting the items, because item order can differ
  pt <- p$plot$theme
  tt <- theme_bw() + theme(text = element_text(colour = 'red'))
  pt <- pt[order(names(pt))]
  tt <- tt[order(names(tt))]
  expect_identical(pt, tt)

  p <- ggplot_build(qplot(1:3, 1:3) + theme(text = element_text(colour = 'red')) + theme_bw())
  expect_true(attr(p$plot$theme, "complete"))
  # Compare the theme objects, after sorting the items, because item order can differ
  pt <- p$plot$theme
  tt <- theme(text = element_text(colour = 'red')) + theme_bw()
  pt <- pt[order(names(pt))]
  tt <- tt[order(names(tt))]
  expect_identical(pt, tt)

  p <- ggplot_build(qplot(1:3, 1:3) + theme(text = element_text(colour = 'red', face = 'italic')))
  expect_false(attr(p$plot$theme, "complete"))
  expect_equal(p$plot$theme$text$colour, "red")
  expect_equal(p$plot$theme$text$face, "italic")

  p <- ggplot_build(qplot(1:3, 1:3) +
    theme(text = element_text(colour = 'red')) +
    theme(text = element_text(face = 'italic')))
  expect_false(attr(p$plot$theme, "complete"))
  expect_equal(p$plot$theme$text$colour, "red")
  expect_equal(p$plot$theme$text$face, "italic")
})

test_that("theme(validate=FALSE) means do not validate_element", {
  p <- qplot(1:3, 1:3)
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
  rect_base <- element_rect(colour = "red", size = 10)
  expect_equal(
    merge_element(element_rect(colour = "blue"), rect_base),
    element_rect(colour = "blue", size = 10)
  )
  line_base <- element_line(colour = "red", size = 10)
  expect_equal(
    merge_element(element_line(colour = "blue"), line_base),
    element_line(colour = "blue", size = 10)
  )
  expect_error(
    merge_element(text_base, rect_base),
    "Only elements of the same class can be merged"
  )
})

test_that("complete plot themes shouldn't inherit from default", {
  default_theme <- theme_gray() + theme(axis.text.x = element_text(colour = "red"))
  base <- qplot(1, 1)

  ptheme <- plot_theme(base + theme(axis.text.x = element_text(colour = "blue")), default_theme)
  expect_equal(ptheme$axis.text.x$colour, "blue")

  ptheme <- plot_theme(base + theme_void(), default_theme)
  expect_null(ptheme$axis.text.x)
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


# Visual tests ------------------------------------------------------------

test_that("aspect ratio is honored", {
  df <- data.frame(x = 1:8, y = 1:8, f = gl(2,4), expand.grid(f1 = 1:2, f2 = 1:2, rep = 1:2))
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
  df <- data.frame(x = 1:3, y = 1:3, z = c("a", "b", "a"), a = 1)
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
  df <- data.frame(x = 1:3, y = 1:3, z = c("a", "b", "a"), a = 1)
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

test_that("strips can be styled independently", {
  df <- data.frame(x = 1:2, y = 1:2)
  plot <- ggplot(df, aes(x, y)) +
    facet_grid(x ~ y) +
    theme(
      strip.background.x = element_rect(fill = "red"),
      strip.background.y = element_rect(fill = "green")
    )
  expect_doppelganger("strip_styling", plot)
})

test_that("rotated axis tick labels work", {
  df <- data.frame(
    y = c(1, 2, 3),
    label = c("short", "medium size", "very long label")
  )

  plot <- ggplot(df, aes(label, y)) + geom_point() +
    theme(axis.text.x = element_text(angle = 50, hjust = 1))
  expect_doppelganger("rotated x axis tick labels", plot)
})
