test_that("plot subclasses retain legacy classes and S7 dispatch", {
  PlotChild <- S7::new_class(
    name = "PlotChild",
    parent = class_ggplot,
    package = NULL
  )
  p <- ggplot() + geom_blank()
  child <- PlotChild(data = p@data, layers = p@layers, mapping = p@mapping)

  expect_s7_class(child, PlotChild)
  expect_s7_class(child, class_gg)
  expect_s3_class(child, "ggplot")
  expect_identical(is_ggplot(child), TRUE)
  expect_identical(child@layers, p@layers)
  expect_s7_class(ggplot_build(child), class_ggplot_built)

  S7::method(`+`, list(PlotChild, S7::class_character)) <- function(e1, e2) e2
  expect_identical(child + "child method", "child method")

  legacy_class <- function(x) UseMethod("legacy_class")
  legacy_class.ggplot <- function(x) "ggplot"
  legacy_class.gg <- function(x) "gg"
  legacy_class.S7_object <- function(x) "S7_object"
  expect_identical(legacy_class(child), "ggplot")
})

test_that("ggplot() throws informative errors", {
  expect_snapshot_error(ggplot(mapping = letters))
  expect_snapshot_error(ggplot(data))
  expect_snapshot_warning(ggplot(NULL, foobar = "nonsense"))
})

test_that("construction have user friendly errors", {
  skip_if(getRversion() < "4.3.0")
  expect_snapshot_error(+ geom_point())
  expect_snapshot_error(geom_point() + geom_bar())
  expect_snapshot_error(ggplot() + 1)
  expect_snapshot_error(ggplot() + geom_point)
})
