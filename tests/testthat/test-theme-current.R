skip_on_cran() # This test suite is long-running (on cran) and is skipped

test_that("current theme can be updated with new elements", {
  old <- set_theme(theme_grey())

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
  expect_null(calc_element("abcde", plot_theme(b1)))

  # element tree gets merged properly
  register_theme_elements(
    abcde = element_text(color = "blue", hjust = 0, vjust = 1),
    element_tree = list(abcde = el_def(element_text, "text"))
  )

  e1 <- calc_element("abcde", plot_theme(b2))
  e2 <- calc_element("text", plot_theme(b2))
  e2@colour <- "blue"
  e2@hjust <- 0
  e2@vjust <- 1
  expect_identical(e1, e2)

  reset_theme_settings()
  set_theme(old)
})

test_that("replacing theme elements with %+replace% operator works", {
  # Changing a "leaf node" works
  t <- theme_grey() %+replace% theme(axis.title.x = element_text(colour = 'red'))
  expect_identical(t$axis.title.x, element_text(colour = 'red'))
  # Make sure the class didn't change or get dropped
  expect_s7_class(t, class_theme)

  # Changing an intermediate node works
  t <- theme_grey() %+replace% theme(axis.title = element_text(colour = 'red'))
  expect_identical(t$axis.title, element_text(colour = 'red'))
  # Descendent is unchanged
  expect_identical(t$axis.title.x, theme_grey()$axis.title.x)

  # Adding empty theme() has no effect
  t <- theme_grey() %+replace% theme()
  expect_identical(t, theme_grey())
})

test_that("set_theme() resets theme to default when called with no arguments", {
  theme_set(theme_void())
  set_theme()
  expect_identical(theme_get(), theme_grey())
})
