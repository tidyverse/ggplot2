# Printing closure environments is stochastic
censor_fun_env <- function(x) {
  x[startsWith(x, "<environment")] <- "<environment: {censored}>"
  x
}

test_that("make_constructor builds a geom constructor", {
  GeomFoo <- ggproto(
    "GeomFoo", Geom,
    draw_panel = function(data, panel_params, coord, my_param = "foo") {
      zeroGrob()
    }
  )
  check <- rlang::exprs(match.arg(my_param, c("foo", "bar")))
  geom_foo <- make_constructor(GeomFoo, checks = check)
  expect_snapshot(print(geom_foo), transform = censor_fun_env)
})

test_that("make_constructor builds a stat constructor", {
  StatFoo <- ggproto(
    "StatFoo", Stat,
    compute_panel = function(data, scales, my_param = "foo") {
      data
    }
  )
  check <- rlang::exprs(match.arg(my_param, c("foo", "bar")))
  stat_foo <- make_constructor(StatFoo, geom = "point", checks = check)
  expect_snapshot(print(stat_foo), transform = censor_fun_env)
})

test_that("make_constructor refuses overdefined cases", {
  # Can't define Geom/Stat twice
  expect_snapshot(
    make_constructor(GeomPoint, geom = "line"),
    error = TRUE
  )
  expect_snapshot(
    make_constructor(StatDensity, geom = "point", stat = "smooth"),
    error = TRUE
  )
})

test_that("make_constructor complains about default values", {
  # No default value for my_param
  GeomFoo <- ggproto(
    "GeomFoo", Geom,
    draw_panel = function(data, panel_params, coord, my_param) {
      zeroGrob()
    }
  )
  expect_snapshot_warning(
    make_constructor(GeomFoo)
  )
  StatFoo <- ggproto(
    "StatFoo", Stat,
    compute_panel = function(data, scales, my_param) {
      data
    }
  )
  expect_snapshot_warning(
    make_constructor(StatFoo, geom = "point")
  )
})

test_that("make_constructor rejects bad input for `checks`", {
  expect_snapshot(
    make_constructor(GeomPoint, checks = 10),
    error = TRUE
  )
  expect_snapshot(
    make_constructor(StatDensity, geom = "line", checks = "A"),
    error = TRUE
  )
})
