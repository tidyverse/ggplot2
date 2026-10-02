skip_on_cran() # This test suite is long-running (on cran) and is skipped

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
  expect_snapshot(merge_element(text_base, rect_base), error = TRUE)
})

test_that("theme elements that don't inherit from element can be combined", {
  expect_identical(combine_elements(1, NULL), 1)
  expect_identical(combine_elements(NULL, 1), 1)
  expect_identical(combine_elements(1, 0), 1)
})

test_that("element tree can be modified", {
  # we cannot add a new theme element without modifying the element tree
  p <- ggplot() + theme(blablabla = element_text(colour = "red"))
  expect_snapshot_warning(print(p))

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
    element_tree = list(blablabla = el_def(element_text, "text"))
  )
  expect_silent(ggplotGrob(p))

  p1 <- ggplot() + theme(blablabla = element_line())
  expect_snapshot_error(ggplotGrob(p1))

  # Expect errors for invalid element trees
  expect_snapshot_error(
    register_theme_elements(element_tree = list(el_def("rect"), el_def("line")))
  )
  expect_snapshot_error(
    register_theme_elements(element_tree = list(foo = "bar"))
  )
  expect_snapshot_error(
    register_theme_elements(element_tree = list(foo = el_def(inherit = "foo")))
  )

  # inheritance and final calculation of novel element works
  final_theme <- ggplot2:::plot_theme(p, theme_gray())
  e1 <- calc_element("blablabla", final_theme)
  e2 <- calc_element("text", final_theme)
  expect_identical(e1@family, e2@family)
  expect_identical(e1@face, e2@face)
  expect_identical(e1@size, e2@size)
  expect_identical(e1@lineheight, e2@lineheight)
  expect_identical(e1@colour, "red") # not inherited from element_text

  # existing elements can be overwritten
  ed <- el_def(element_rect, "rect")
  register_theme_elements(
    element_tree = list(axis.title = ed)
  )
  expect_identical(get_element_tree()$axis.title, ed)

  reset_theme_settings() # revert back to defaults
})

test_that("element_text throws appropriate conditions", {
  expect_snapshot_warning(
    element_text(colour = c("red", "blue"))
  )
  expect_snapshot_warning(
    element_text(margin = unit(1, "cm"))
  )
  expect_snapshot(
    element_text(margin = 5),
    error = TRUE
  )
  expect_snapshot(
    element_text(colour = sqrt(2)),
    error = TRUE
  )

  # Some absurd case found in reverse dependency check where
  # labs(y = element_blank()) for some reason
  el <- theme_get()$text
  expect_snapshot(
    x <- element_grob(el, label = element_blank())
  )
})

test_that("Minor tick length supports biparental inheritance", {
  my_theme <- theme_gray() + theme(
    axis.ticks.length = unit(1, "cm"),
    axis.ticks.length.y.left = unit(1, "pt"),
    axis.minor.ticks.length.y = unit(1, "inch"),
    axis.minor.ticks.length = rel(0.5)
  )
  expect_equal( # Inherits rel(0.5) from minor, 1cm from major
    calc_element("axis.minor.ticks.length.x.bottom", my_theme),
    unit(1, "cm") * 0.5
  )
  expect_equal( # Inherits 1inch directly from minor
    calc_element("axis.minor.ticks.length.y.left", my_theme),
    unit(1, "inch")
  )
})

test_that("geom elements are inherited correctly", {

  GeomFoo <- ggproto("GeomFoo", GeomPoint)
  GeomBar <- ggproto("GeomBar", GeomFoo)

  p <- ggplot(data.frame(x = 1), aes(x, x)) +
    stat_identity(geom = GeomBar) +
    theme(
      geom = element_geom(pointshape = 15),
      geom.point = element_geom(borderwidth = 2, ink = "blue"),
      geom.foo = element_geom(pointsize = 2),
      geom.bar = element_geom(ink = "red")
    )
  p <- layer_data(p)
  expect_equal(p$shape, 15)
  expect_equal(p$stroke, 2)
  expect_equal(p$size, 2)
  expect_equal(p$colour, "red")
})

# Visual tests ------------------------------------------------------------

test_that("element_polygon() can render a grob", {

  t <- theme_gray() + theme(polygon = element_polygon(fill = "orchid"))
  e <- calc_element("polygon", t)
  g <- element_grob(
    e,
    x  = c(0, 0.5, 1, 0.5, 0.15, 0.85, 0.85, 0.15),
    y  = c(0.5, 0, 0.5, 1, 0.15, 0.15, 0.85, 0.85),
    id = c(1, 1, 1, 1, 2, 2, 2, 2),
    colour = c("orange", "limegreen")
  )

  expect_s3_class(g, "pathgrob")
  expect_equal(g$gp$fill, "orchid")

  expect_doppelganger(
    "polygon elements",
    function() {grid.newpage(); grid.draw(g)}
  )
})

test_that("element_point() can render a grob", {

  t <- theme_gray() + theme(point = element_point(shape = 21, size = 5))
  e <- calc_element("point", t)
  g <- element_grob(
    e,
    x = seq(0.1, 0.9, length.out = 5),
    y = seq(0.9, 0.1, length.out = 5),
    fill = c("orange", "limegreen", "orchid", "turquoise", "grey")
  )

  expect_s3_class(g, "points")
  expect_equal(g$pch, 21)

  expect_doppelganger(
    "point elements",
    function() {grid.newpage(); grid.draw(g)}
  )
})

