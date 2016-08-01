context("Themes")

test_that("Modifying theme element properties with + operator", {

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


test_that("Adding theme object to ggplot object with + operator", {

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


test_that("Replacing theme elements with %+replace% operator", {
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


test_that("Calculating theme element inheritance", {
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


test_that("Complete and non-complete themes interact correctly with each other", {
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


test_that("Complete and non-complete themes interact correctly with ggplot objects", {
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

test_that("All elements in complete themes have inherit.blank=TRUE", {
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


# Visual tests ------------------------------------------------------------

test_that("aspect ratio is honored", {
  p <- ggplot(data.frame(x = 1:8, y = 1:8, f = gl(2,4), expand.grid(f1 = 1:2, f2 = 1:2, rep = 1:2)), aes(x, y)) + geom_point()

  vdiffr::expect_doppelganger(
    p + theme(aspect.ratio = 3),
    "height is 3 times width"
  )
  vdiffr::expect_doppelganger(
    p + facet_wrap(~f) + theme(aspect.ratio = 3),
    "height is 3 times width, 2 wrap facets"
  )
  vdiffr::expect_doppelganger(
    p + facet_grid(.~f) + theme(aspect.ratio = 3),
    "height is 3 times width, 2 column facets"
  )
  vdiffr::expect_doppelganger(
    p + facet_grid(f~.) + theme(aspect.ratio = 3),
    "height is 3 times width, 2 row facets"
  )
  vdiffr::expect_doppelganger(
    p + facet_grid(f1~f2) + theme(aspect.ratio = 3),
    "height is 3 times width, 2x2 facets"
  )

  vdiffr::expect_doppelganger(
    p + theme(aspect.ratio = 1/3),
    "width is 3 times height"
  )
  vdiffr::expect_doppelganger(
    p + facet_wrap(~f) + theme(aspect.ratio = 1/3),
    "width is 3 times height, 2 wrap facets"
  )
  vdiffr::expect_doppelganger(
    p + facet_grid(.~f) + theme(aspect.ratio = 1/3),
    "width is 3 times height, 2 column facets"
  )
  vdiffr::expect_doppelganger(
    p + facet_grid(f~.) + theme(aspect.ratio = 1/3),
    "width is 3 times height, 2 row facets"
  )
  vdiffr::expect_doppelganger(
    p + facet_grid(f1~f2) + theme(aspect.ratio = 1/3),
    "width is 3 times height, 2x2 facets"
  )
})

test_that("themes are drawn in the right style", {
  library(grid)
  p <- qplot(1:3, 1:3)

  # Tests for adding theme objects together
  # Some of these add directly to ggplot object; others add to theme object first
  vdiffr::expect_doppelganger(
    p + theme_bw() + theme(text = element_text(colour = 'blue')),
    "theme_bw() plus blue text"
  )

  t <- theme_bw() + theme(text = element_text(colour = 'blue'))
  vdiffr::expect_doppelganger(
    p + t,
    "add saved theme object with theme_bw() plus blue text"
  )
  vdiffr::expect_doppelganger(
    p + theme(text = element_text(colour = 'blue')) + theme_bw(),
    "blue text plus theme_bw() - result is black text"
  )

  t <- theme(text = element_text(colour = 'blue')) + theme_bw()
  vdiffr::expect_doppelganger(
    p + t,
    "add saved theme object with blue text plus theme_bw()) - result is black text"
  )
  vdiffr::expect_doppelganger(
    p + theme(text = element_text(colour = 'blue', face = 'italic')),
    "add blue and italic in single element object"
  )
  vdiffr::expect_doppelganger(
    p + theme(
      text = element_text(colour = 'blue')) +
      theme(text = element_text(face = 'italic')
      ),
    "add blue and italic in separate element objects"
  )
  vdiffr::expect_doppelganger(
    p + theme(
      text = element_text(colour = 'blue'),
      text = element_text(face = 'italic')
    ),
    "add blue and italic in one theme object with two 'text' elements - result is blue only"
  )

  # Inheritance tests
  vdiffr::expect_doppelganger(
    p + theme_bw(base_size = 24, base_family = "Times") + labs(title = "Title text here"),
    'add theme_bw(base_size=24, base_family="Times")'
  )
  vdiffr::expect_doppelganger(
    p + theme_bw() +
      theme(axis.title   = element_text(size = rel(2), colour = 'blue')) +
      theme(axis.title.x = element_text(size = rel(2))),
    "axis title text is blue, compounded relative sizing"
  )

  # Next two tests contrast the + operator with the %+replace% operator
  t <- theme_bw() + theme(axis.title.y = element_text(size = rel(2)))
  vdiffr::expect_doppelganger(
    p + t,
    "theme_bw + larger relative size for axis.title.y"
  )

  t <- theme_bw() %+replace% theme(axis.title.y = element_text(size = rel(2)))
  vdiffr::expect_doppelganger(
    p + t,
    "theme_bw %+replace% larger relative size for axis.title.y - result is angle=0"
  )

  t <- theme_bw() + theme(text = element_blank())
  vdiffr::expect_doppelganger(
    p + t,
    "text is element_blank - result is no text"
  )

  # Testing specific elements
  vdiffr::expect_doppelganger(
    p + theme(axis.text = element_blank(), axis.ticks = element_blank(),
              axis.title = element_blank(),
              plot.background = element_rect(fill = "lightblue"),
              panel.border = element_rect(colour = "black", size = 4, fill = NA)),
    "many blank items, and light blue plot background"
  )
})
