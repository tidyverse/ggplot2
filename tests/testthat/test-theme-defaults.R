skip_on_cran() # This test suite is long-running (on cran) and is skipped

test_that("all elements in complete themes have inherit.blank=TRUE", {
  inherit_blanks <- function(theme) {
    all(vapply(
      theme, try_prop,
      name = "inherit.blank", default = TRUE,
      logical(1)
    ))
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

test_that("complete plot themes shouldn't inherit from default", {
  default_theme <- theme_gray() + theme(axis.text.x = element_text(colour = "red"))
  base <- ggplot(data.frame(x = 1), aes(x, x)) + geom_point()

  ptheme <- plot_theme(base + theme(axis.text.x = element_text(colour = "blue")), default_theme)
  expect_equal(ptheme$axis.text.x@colour, "blue")

  ptheme <- plot_theme(base + theme_void(), default_theme)
  expect_null(ptheme$axis.text.x)
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

test_that("header_family is passed on correctly", {

  td <- theme_dark(base_family = "x", header_family = "y")

  test <- calc_element("plot.title", td)
  expect_equal(test@family, "y")

  test <- calc_element("plot.subtitle", td)
  expect_equal(test@family, "x")
})

# Visual tests ------------------------------------------------------------

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

test_that("theme ink and paper settings work", {

  p <- ggplot(mpg, aes(displ, hwy, colour = drv)) +
    geom_point() +
    facet_wrap(~"Strip title") +
    labs(
      title = "Main title",
      subtitle = "Subtitle",
      tag = "A",
      caption = "Caption"
    )

  expect_doppelganger(
    "Theme with inverted colours",
    p + theme_gray(ink = "white", paper = "black")
  )
})
