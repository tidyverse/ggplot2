test_that("ranges update only for variables listed in aesthetics", {
  sc <- scale_alpha()

  sc$train_df(data_frame(alpha = 1:10))
  expect_equal(sc$range$range, c(1, 10))

  sc$train_df(data_frame(alpha = 50))
  expect_equal(sc$range$range, c(1, 50))

  sc$train_df(data_frame(beta = 100))
  expect_equal(sc$range$range, c(1, 50))

  sc$train_df(data_frame())
  expect_equal(sc$range$range, c(1, 50))
})

test_that("mapping works", {
  sc <- scale_alpha(range = c(0, 1), na.value = 0)
  sc$train_df(data_frame(alpha = 1:10))

  expect_equal(
    sc$map_df(data_frame(alpha = 1:10))[[1]],
    seq(0, 1, length.out = 10)
  )

  expect_equal(sc$map_df(data_frame(alpha = NA))[[1]], 0)

  expect_equal(
    sc$map_df(data_frame(alpha = c(-10, 11)))[[1]],
    c(0, 0))
})

test_that("aesthetics can be set independently of scale name", {
  df <- data_frame(
    x = LETTERS[1:3],
    y = LETTERS[4:6]
  )
  p <- ggplot(df, aes(x, y, fill = y)) +
    scale_colour_manual(values = c("red", "green", "blue"), aesthetics = "fill")

  expect_equal(get_layer_data(p)$fill, c("red", "green", "blue"))
})

test_that("multiple aesthetics can be set with one function call", {
  df <- data_frame(
    x = LETTERS[1:3],
    y = LETTERS[4:6]
  )
  p <- ggplot(df, aes(x, y, colour = x, fill = y)) +
    scale_colour_manual(
      values = c("grey20", "grey40", "grey60", "red", "green", "blue"),
      aesthetics = c("colour", "fill")
    )

  expect_equal(get_layer_data(p)$colour, c("grey20", "grey40", "grey60"))
  expect_equal(get_layer_data(p)$fill, c("red", "green", "blue"))

  # color order is determined by data order, and breaks are combined where possible
  df <- data_frame(
    x = LETTERS[1:3],
    y = LETTERS[2:4]
  )
  p <- ggplot(df, aes(x, y, colour = x, fill = y)) +
    scale_colour_manual(
      values = c("cyan", "red", "green", "blue"),
      aesthetics = c("fill", "colour")
    )

  expect_equal(get_layer_data(p)$colour, c("cyan", "red", "green"))
  expect_equal(get_layer_data(p)$fill, c("red", "green", "blue"))
})

test_that("scales accept lambda notation for function input", {
  check_lambda <- function(items, ggproto) {
    vapply(items, function(x) {
      f <- environment(ggproto[[x]])$f
      is_lambda(f)
    }, logical(1))
  }

  # Test continuous scale
  scale <- scale_fill_gradient(
    limits = ~ .x + c(-1, 1),
    breaks = ~ seq(.x[1], .x[2], by = 2),
    minor_breaks = ~ seq(.x[1], .x[2], by = 1),
    labels = ~ toupper(.x),
    rescaler = ~ rescale_mid(.x, mid = 0),
    oob = ~ oob_squish(.x, .y, only.finite = FALSE)
  )
  check <- check_lambda(
    c("limits", "breaks", "minor_breaks", "labels", "rescaler"),
    scale
  )
  expect_true(all(check))

  # Test discrete scale
  scale <- scale_x_discrete(
    limits = ~ rev(.x),
    breaks = ~ .x[-1],
    labels = ~ toupper(.x)
  )
  check <- check_lambda(c("limits", "breaks", "labels"), scale)
  expect_true(all(check))

  # Test binned scale
  scale <- scale_fill_steps(
    limits = ~ .x + c(-1, 1),
    breaks = ~ seq(.x[1], .x[2], by = 2),
    labels = ~ toupper(.x),
    rescaler = ~ rescale_mid(.x, mid = 0),
    oob = ~ oob_squish(.x, .y, only.finite = FALSE)
  )
  check <- check_lambda(
    c("limits", "breaks", "labels", "rescaler"),
    scale
  )
  expect_true(all(check))
})

test_that("training incorrectly appropriately communicates the offenders", {

  sc <- scale_colour_viridis_d()
  expect_snapshot_error(
    sc$train(1:5)
  )

  sc <- scale_colour_viridis_c()
  expect_snapshot_error(
    sc$train(LETTERS[1:5])
  )
})

test_that("Using `scale_name` prompts deprecation message", {

  expect_snapshot_warning(continuous_scale("x", "foobar", pal_identity()))
  expect_snapshot_warning(discrete_scale("x",   "foobar", pal_identity()))
  expect_snapshot_warning(binned_scale("x",     "foobar", pal_identity()))

})

# Continuous scales -------------------------------------------------------

test_that("limits with NA are replaced with the min/max of the data for continuous scales", {
  make_scale <- function(limits = NULL, data = NULL) {
    scale <- continuous_scale("aesthetic", palette = identity, limits = limits)
    if (!is.null(data)) {
      scale$train(data)
    }
    scale
  }

  # emptiness
  expect_true(make_scale()$is_empty())
  expect_false(make_scale(limits = c(0, 1))$is_empty())
  expect_true(make_scale(limits = c(0, NA))$is_empty())
  expect_true(make_scale(limits = c(NA, NA))$is_empty())
  expect_true(make_scale(limits = c(NA, 0))$is_empty())

  # limits
  expect_equal(make_scale(data = 1:5)$get_limits(), c(1, 5))
  expect_equal(make_scale(limits = c(1, 5))$get_limits(), c(1, 5))
  expect_equal(make_scale(limits = c(NA, NA))$get_limits(), c(0, 1))
  expect_equal(make_scale(limits = c(NA, NA), data = 1:5)$get_limits(), c(1, 5))
  expect_equal(make_scale(limits = c(1, NA), data = 1:5)$get_limits(), c(1, 5))
  expect_equal(make_scale(limits = c(NA, 5), data = 1:5)$get_limits(), c(1, 5))
})


test_that("continuous scales warn about faulty `limits`", {
  expect_snapshot(scale_x_continuous(limits = c("A", "B")), error = TRUE)
  expect_snapshot(scale_x_continuous(limits = 1:3), error = TRUE)
})

# Discrete scales ---------------------------------------------------------

# From #5623
test_that("Discrete scales with only NAs return `na.value`", {

  x <- c(NA, NA)

  sc <- scale_colour_discrete(na.value = "red")
  sc$train(x)
  expect_equal(sc$map(x), c("red", "red"))

  sc <- scale_shape(na.value = NA_real_)
  sc$train(x)
  expect_equal(sc$map(x), c(NA_real_, NA_real_))
})

test_that("discrete scales work with NAs in arbitrary positions", {
  # Prevents intermediate caching of palettes
  map <- function(x, limits) {
    sc <- scale_colour_manual(
      values = c("red", "green", "blue"),
      na.value = "gray"
    )
    sc$map(x, limits)
  }

  # All inputs should yield output regardless of where NA is
  input  <- c("A", "B", "C", NA)
  output <- c("red", "green", "blue", "gray")

  test <- map(input, limits = c("A", "B", "C", NA))
  expect_equal(test, output)

  test <- map(input, limits = c("A", NA, "B", "C"))
  expect_equal(test, output)

  test <- map(input, limits = c(NA, "A", "B", "C"))
  expect_equal(test, output)

})

test_that("discrete scales can map to 2D structures", {

  p <- ggplot(mtcars, aes(disp, mpg, colour = factor(cyl))) +
    geom_point()

  # Test it can map to a vctrs rcrd class
  rcrd <- new_rcrd(list(a = LETTERS[1:3], b = 3:1))

  ld <- layer_data(p + scale_colour_manual(values = rcrd, na.value = NA))
  expect_s3_class(ld$colour, "vctrs_rcrd")
  expect_length(ld$colour, nrow(mtcars))

  # Test it can map to data.frames
  df <- data_frame0(a = LETTERS[1:3], b = 3:1)
  my_pal <- function(n) vec_slice(df, seq_len(n))

  ld <- layer_data(p + discrete_scale("colour", palette = my_pal))
  expect_s3_class(ld$colour, "data.frame")
  expect_equal(dim(ld$colour), c(nrow(mtcars), ncol(df)))

  # Test it can map to matrices
  mtx <- cbind(a = LETTERS[1:3], b = LETTERS[4:6])
  my_pal <- function(n) vec_slice(mtx, seq_len(n))

  ld <- layer_data(p + discrete_scale("colour", palette = my_pal))
  expect_true(is.matrix(ld$colour))
  expect_equal(dim(ld$colour), c(nrow(mtcars), ncol(df)))
})

# Calls -------------------------------------------------------------------

test_that("scale functions accurately report their calls", {

  construct <- exprs(
    scale_alpha(),
    scale_alpha_binned(),
    scale_alpha_continuous(),
    scale_alpha_date(),
    scale_alpha_datetime(),
    scale_alpha_discrete(),
    scale_alpha_identity(),
    scale_alpha_manual(),
    scale_alpha_ordinal(),
    # Skipping American spelling of 'color' scales here
    scale_colour_binned(),
    scale_colour_brewer(),
    scale_colour_continuous(),
    scale_colour_date(),
    scale_colour_datetime(),
    scale_colour_discrete(),
    scale_colour_distiller(),
    scale_colour_fermenter(),
    scale_colour_gradient(),
    scale_colour_gradient2(),
    # Some scales have required arguments
    scale_colour_gradientn(colours = c("firebrick", "limegreen")),
    scale_colour_grey(),
    scale_colour_hue(),
    scale_colour_identity(),
    scale_colour_manual(),
    scale_colour_ordinal(),
    scale_colour_steps(),
    scale_colour_steps2(),
    scale_colour_stepsn(colours = c("orchid", "tomato")),
    scale_colour_viridis_b(),
    scale_colour_viridis_c(),
    scale_colour_viridis_d(),
    scale_continuous_identity(aesthetics = "foo"),
    scale_discrete_identity(aesthetics = "bar"),
    scale_discrete_manual(aesthetics = "baz"),
    scale_fill_binned(),
    scale_fill_brewer(),
    scale_fill_continuous(),
    scale_fill_date(),
    scale_fill_datetime(),
    scale_fill_discrete(),
    scale_fill_distiller(),
    scale_fill_fermenter(),
    scale_fill_gradient(),
    scale_fill_gradient2(),
    scale_fill_gradientn(colours = c("yellow", "green")),
    scale_fill_grey(),
    scale_fill_hue(),
    scale_fill_identity(),
    scale_fill_manual(),
    scale_fill_ordinal(),
    scale_fill_steps(),
    scale_fill_steps2(),
    scale_fill_stepsn(colours = c("steelblue", "pink")),
    scale_fill_viridis_b(),
    scale_fill_viridis_c(),
    scale_fill_viridis_d(),
    scale_linetype(),
    scale_linetype_binned(),
    # scale_linetype_continuous(), # designed to throw error
    scale_linetype_discrete(),
    scale_linetype_identity(),
    scale_linetype_manual(),
    scale_linewidth(),
    scale_linewidth_binned(),
    scale_linewidth_continuous(),
    scale_linewidth_date(),
    scale_linewidth_datetime(),
    scale_linewidth_discrete(),
    scale_linewidth_identity(),
    scale_linewidth_manual(),
    scale_linewidth_ordinal(),
    scale_radius(),
    scale_shape(),
    scale_shape_binned(),
    # scale_shape_continuous(), # designed to throw error
    scale_shape_discrete(),
    scale_shape_identity(),
    scale_shape_manual(),
    scale_shape_ordinal(),
    scale_size(),
    scale_size_area(),
    scale_size_binned(),
    scale_size_binned_area(),
    scale_size_continuous(),
    scale_size_date(),
    scale_size_datetime(),
    scale_size_discrete(),
    scale_size_identity(),
    scale_size_manual(),
    scale_size_ordinal(),
    scale_x_binned(),
    scale_x_continuous(),
    scale_x_date(),
    scale_x_datetime(),
    scale_x_discrete(),
    scale_x_log10(),
    scale_x_reverse(),
    scale_x_sqrt(),
    # scale_x_time(),
    scale_y_binned(),
    scale_y_continuous(),
    scale_y_date(),
    scale_y_datetime(),
    scale_y_discrete(),
    scale_y_log10(),
    scale_y_reverse(),
    scale_y_sqrt(),
    # scale_y_time(),
    xlim(10, 20),
    ylim("A", "B")
  )
  if (is_installed("hms")) {
    construct <- c(construct, exprs(scale_x_time(), scale_y_time()))
  }

  suppressWarnings(
    calls <- lapply(construct, function(x) eval(x)$call)
  )
  expect_equal(calls, construct)
})

test_that("scale call is found accurately", {

  call_template <- quote(scale_x_continuous(transform = "log10"))

  sc <- do.call("scale_x_continuous", list(transform = "log10"))
  expect_equal(sc$call, call_template)

  sc <- inject(scale_x_continuous(!!!list(transform = "log10")))
  expect_equal(sc$call, call_template)

  sc <- exec("scale_x_continuous", transform = "log10")
  expect_equal(sc$call, call_template)

  foo <- function() scale_x_continuous(transform = "log10")
  expect_equal(foo()$call, call_template)

  env <- new_environment()
  env$bar <- function() scale_x_continuous(transform = "log10")
  expect_equal(env$bar()$call, call_template)

  # Now should recognise the outer function
  scale_x_new <- function() {
    scale_x_continuous(transform = "log10")
  }
  expect_equal(
    scale_x_new()$call,
    quote(scale_x_new())
  )
})


# Labels and breaks -------------------------------------------------------

test_that("breaks and labels are correctly checked", {
  expect_snapshot_error(check_breaks_labels(1:10, letters))
  expect_snapshot_error(scale_x_continuous(breaks = NA))
  p <- ggplot(mtcars) + geom_point(aes(mpg, disp)) + scale_x_continuous(minor_breaks = NA)
  expect_snapshot_error(ggplot_build(p))
  p <- ggplot(mtcars) + geom_point(aes(mpg, disp)) + scale_x_continuous(labels = NA)
  expect_snapshot_error(ggplotGrob(p))
  p <- ggplot(mtcars) + geom_point(aes(mpg, disp)) + scale_x_continuous(labels = function(x) 1:2)
  expect_snapshot_error(ggplotGrob(p))
  expect_snapshot_error(scale_x_discrete(breaks = NA))
  p <- ggplot(mtcars) + geom_bar(aes(factor(gear))) + scale_x_discrete(labels = NA)
  expect_snapshot_error(ggplotGrob(p))

  expect_snapshot_error(scale_x_binned(breaks = NA))
  p <- ggplot(mtcars) + geom_bar(aes(mpg)) + scale_x_binned(labels = NA)
  expect_snapshot_error(ggplotGrob(p))
  p <- ggplot(mtcars) + geom_bar(aes(mpg)) + scale_x_binned(labels = function(x) 1:2)
  expect_snapshot_error(ggplotGrob(p))
})

test_that("labels match breaks, even when outside limits", {
  sc <- scale_y_continuous(breaks = 1:4, labels = 1:4, limits = c(1, 3))

  expect_equal(sc$get_breaks(), 1:4)
  expect_equal(sc$get_labels(), 1:4)
  expect_equal(sc$get_breaks_minor(), c(1, 1.5, 2, 2.5, 3))
})

test_that("labels match breaks", {
  expect_snapshot(scale_x_discrete(breaks = 1:3, labels = 1:2), error = TRUE)
  expect_snapshot(scale_x_continuous(breaks = 1:3, labels = 1:2), error = TRUE)
})

test_that("labels don't have to match null breaks", {
  expect_silent(check_breaks_labels(breaks = 1:3, labels = NULL))
  expect_silent(check_breaks_labels(breaks = NULL, labels = 1:2))
})

test_that("labels accept expressions", {
  labels <- parse(text = paste0(1:4, "^degree"))
  sc <- scale_y_continuous(breaks = 1:4, labels = labels, limits = c(1, 3))

  expect_equal(sc$get_breaks(), 1:4)
  expect_equal(sc$get_labels(), as.list(labels))
})

test_that("labels don't have extra spaces", {
  labels <- c("a", "abc", "abcdef")

  sc1 <- scale_x_discrete(limits = labels)
  sc2 <- scale_fill_discrete(limits = labels)

  expect_equal(sc1$get_labels(), labels)
  expect_equal(sc2$get_labels(), labels)
})

test_that("out-of-range breaks are dropped", {

  # Limits are explicitly specified, automatic labels
  sc <- scale_x_continuous(breaks = 1:5, limits = c(2, 4))
  bi <- sc$break_info()
  expect_equal(bi$labels, as.character(2:4))
  expect_equal(bi$major, c(0, 0.5, 1))
  expect_equal(bi$major_source, 2:4)

  # Limits and labels are explicitly specified
  sc <- scale_x_continuous(breaks = 1:5, labels = letters[1:5], limits = c(2, 4))
  bi <- sc$break_info()
  expect_equal(bi$labels, letters[2:4])
  expect_equal(bi$major, c(0, 0.5, 1))
  expect_equal(bi$major_source, 2:4)

  # Limits are specified, and all breaks are out of range
  sc <- scale_x_continuous(breaks = c(1,5), labels = letters[c(1,5)], limits = c(2, 4))
  bi <- sc$break_info()
  expect_length(bi$labels, 0)
  expect_length(bi$major, 0)
  expect_length(bi$major_source, 0)

  # limits aren't specified, automatic labels
  # limits are set by the data
  sc <- scale_x_continuous(breaks = 1:5)
  sc$train_df(data_frame(x = 2:4))
  bi <- sc$break_info()
  expect_equal(bi$labels, as.character(2:4))
  expect_equal(bi$major_source, 2:4)
  expect_equal(bi$major, c(0, 0.5, 1))

  # Limits and labels are specified
  sc <- scale_x_continuous(breaks = 1:5, labels = letters[1:5])
  sc$train_df(data_frame(x = 2:4))
  bi <- sc$break_info()
  expect_equal(bi$labels, letters[2:4])
  expect_equal(bi$major_source, 2:4)
  expect_equal(bi$major, c(0, 0.5, 1))

  # Limits aren't specified, and all breaks are out of range of data
  sc <- scale_x_continuous(breaks = c(1,5), labels = letters[c(1,5)])
  sc$train_df(data_frame(x = 2:4))
  bi <- sc$break_info()
  expect_length(bi$labels, 0)
  expect_length(bi$major, 0)
  expect_length(bi$major_source, 0)
})

test_that("no minor breaks when only one break", {
  sc1 <- scale_x_discrete(limits = "a")
  sc2 <- scale_x_continuous(limits = c(1, 1))

  expect_length(sc1$get_breaks_minor(), 0)
  expect_length(sc2$get_breaks_minor(), 0)
})

init_scale <- function(...) {
  sc <- scale_x_discrete(...)
  sc$train(factor(1:100))
  expect_length(sc$get_limits(), 100)
  sc
}

test_that("discrete labels match breaks", {

  sc <- init_scale(breaks = 0:5 * 10)
  expect_length(sc$get_breaks(), 5)
  expect_length(sc$get_labels(), 5)
  expect_equal(sc$get_labels(), sc$get_breaks(), ignore_attr = TRUE)

  sc <- init_scale(breaks = 0:5 * 10, labels = letters[1:6])
  expect_length(sc$get_breaks(), 5)
  expect_length(sc$get_labels(), 5)
  expect_equal(sc$get_labels(), letters[2:6])

  sc <- init_scale(breaks = 0:5 * 10, labels =
    function(x) paste(x, "-", sep = ""))
  expect_equal(sc$get_labels(), c("10-", "20-", "30-", "40-", "50-"))

  pick_5 <- function(x) sample(x, 5)
  sc <- init_scale(breaks = pick_5)
  expect_length(sc$get_breaks(), 5)
  expect_length(sc$get_labels(), 5)
})

test_that("scale breaks work with numeric log transformation", {
  sc <- scale_x_continuous(limits = c(1, 1e5), transform = transform_log10())
  expect_equal(sc$get_breaks(), c(0, 2, 4)) # 1, 100, 10000
  expect_equal(sc$get_breaks_minor(), c(0, 1, 2, 3, 4, 5))
})

test_that("continuous scales with no data have no breaks or labels", {
  sc <- scale_x_continuous()

  expect_equal(sc$get_breaks(), numeric())
  expect_equal(sc$get_labels(), character())
  expect_equal(sc$get_limits(), c(0, 1))
})

test_that("discrete scales with no data have no breaks or labels", {
  sc <- scale_x_discrete()

  expect_equal(sc$get_breaks(), numeric())
  expect_equal(sc$get_labels(), character())
  expect_equal(sc$get_limits(), c(0, 1))
})

test_that("passing continuous limits to a discrete scale generates a warning", {
  expect_snapshot_warning(scale_x_discrete(limits = 1:3))
})

test_that("suppressing breaks, minor_breask, and labels works", {
  expect_null(scale_x_continuous(breaks = NULL, limits = c(1, 3))$get_breaks())
  expect_null(scale_x_discrete(breaks = NULL, limits = c("one", "three"))$get_breaks())
  expect_null(scale_x_continuous(minor_breaks = NULL, limits = c(1, 3))$get_breaks_minor())

  expect_null(scale_x_continuous(labels = NULL, limits = c(1, 3))$get_labels())
  expect_null(scale_x_discrete(labels = NULL, limits = c("one", "three"))$get_labels())

  # date, datetime
  lims <- as.Date(c("2000/1/1", "2000/2/1"))
  expect_null(scale_x_date(breaks = NULL, limits = lims)$get_breaks())
  # NA is defunct, should throw error
  expect_snapshot(
    scale_x_date(breaks = NA, limits = lims)$get_breaks(),
    error = TRUE
  )
  expect_null(scale_x_date(labels = NULL, limits = lims)$get_labels())
  expect_snapshot(
    scale_x_date(labels = NA, limits = lims)$get_labels(),
    error = TRUE
  )
  expect_null(scale_x_date(minor_breaks = NULL, limits = lims)$get_breaks_minor())
  expect_snapshot(
    scale_x_date(minor_breaks = NA, limits = lims)$get_breaks_minor(),
    error = TRUE
  )

  # date, datetime
  lims <- as.POSIXct(c("2000/1/1 0:0:0", "2010/1/1 0:0:0"))
  expect_null(scale_x_datetime(breaks = NULL, limits = lims)$get_breaks())
  expect_snapshot(
    scale_x_datetime(breaks = NA, limits = lims)$get_breaks(),
    error = TRUE
  )
  expect_null(scale_x_datetime(labels = NULL, limits = lims)$get_labels())
  expect_snapshot(
    scale_x_datetime(labels = NA, limits = lims)$get_labels(),
    error = TRUE
  )
  expect_null(scale_x_datetime(minor_breaks = NULL, limits = lims)$get_breaks_minor())
  expect_snapshot(
    scale_x_datetime(minor_breaks = NA, limits = lims)$get_breaks_minor(),
    error = TRUE
  )
})

test_that("scale_breaks with explicit NA options (deprecated)", {
  # NA is defunct, should throw error
  expect_error(scale_x_continuous(breaks = NA))
  expect_error(scale_y_continuous(breaks = NA))
  expect_error(scale_alpha_continuous(breaks = NA))
  expect_error(scale_size_continuous(breaks = NA))
  expect_error(scale_fill_continuous(breaks = NA))
  expect_error(scale_colour_continuous(breaks = NA))
})

test_that("breaks can be specified by names of labels", {
  labels <- setNames(LETTERS[1:4], letters[1:4])

  s <- scale_x_discrete(limits = letters[1:4], labels = labels)
  expect_equal(as.vector(s$get_breaks()), letters[1:4])
  expect_equal(as.vector(s$get_labels()), LETTERS[1:4])

  s <- scale_x_discrete(limits = letters[1:4], labels = rev(labels))
  expect_equal(as.vector(s$get_breaks()), letters[1:4])
  expect_equal(as.vector(s$get_labels()), LETTERS[1:4])

  s <- scale_x_discrete(limits = letters[1:4], labels = labels[1:2])
  expect_equal(as.vector(s$get_breaks()), letters[1:4])
  expect_equal(as.vector(s$get_labels()), c("A", "B", "c", "d"))

  s <- scale_x_discrete(limits = letters[1:4], labels = labels[3:4])
  expect_equal(as.vector(s$get_breaks()), letters[1:4])
  expect_equal(as.vector(s$get_labels()), c("a", "b", "C", "D"))

  s <- scale_x_discrete(limits = letters[1:3], labels = labels)
  expect_equal(as.vector(s$get_breaks()), letters[1:3])
  expect_equal(as.vector(s$get_labels()), LETTERS[1:3])
})

test_that("only finite or NA values for breaks for transformed scales (#871)", {
  sc <- scale_y_continuous(limits = c(0.01, 0.99), transform = "probit",
                           breaks = seq(0, 1, 0.2))
  breaks <- sc$break_info()$major_source
  expect_true(all(is.finite(breaks) | is.na(breaks)))
})

test_that("minor breaks are transformed by scales", {
  sc <- scale_y_continuous(limits = c(1, 100), transform = "log10",
    minor_breaks = c(1, 10, 100))

  expect_equal(sc$get_breaks_minor(), c(0, 1, 2))
})

test_that("continuous limits accepts functions", {
  p <- ggplot(mpg, aes(class, hwy)) +
    scale_y_continuous(limits = function(lims) (c(lims[1] - 10, lims[2] + 100)))

  expect_equal(
    get_panel_scales(p)$y$get_limits(),
    c(range(mpg$hwy)[1] - 10, range(mpg$hwy)[2] + 100)
  )
})

test_that("equal length breaks and labels can be passed to ViewScales with limits", {

  test_scale <- scale_x_continuous(
    breaks = c(0, 20, 40),
    labels = c("0", "20", "40"),
    limits = c(10, 30)
  )

  expect_identical(test_scale$get_breaks(), c(0, 20, 40))
  expect_identical(test_scale$get_labels(), c(c("0", "20", "40")))

  test_view_scale <- view_scale_primary(test_scale)
  expect_identical(test_view_scale$get_breaks(), c(NA, 20, NA))
  expect_identical(test_view_scale$get_labels(), c(c("0", "20", "40")))

  # ViewScale accepts the limits in the opposite order (#3952)
  test_view_scale_rev <- view_scale_primary(test_scale, limits = rev(test_scale$get_limits()))
  expect_identical(test_view_scale_rev$get_breaks(), c(NA, 20, NA))
  expect_identical(test_view_scale_rev$get_labels(), c(c("0", "20", "40")))
})

test_that("break names are returned as labels", {

  sc <- scale_x_continuous(breaks = c(A = 10, B = 20, C = 30))
  sc$train(c(10, 30))
  expect_equal(sc$get_labels(), c("A", "B", "C"))

  sc <- scale_x_discrete(breaks = c(foo = "A", bar = "B", qux = "C"))
  sc$train(c(LETTERS[1:3]))
  expect_equal(sc$get_labels(), c("foo", "bar", "qux"))
})

test_that("numeric scale transforms can produce breaks", {

  test_breaks <- function(transform, limits) {
    scale <- scale_x_continuous(transform = transform)
    scale$train(scale$transform(limits))
    view <- view_scale_primary(scale)
    scale$get_transformation()$inverse(view$get_breaks())
  }

  expect_snapshot(test_breaks("asn", limits = c(0, 1)))
  expect_snapshot(test_breaks("sqrt", limits = c(0, 10)))
  expect_snapshot(test_breaks("atanh", limits = c(-0.9, 0.9)))
  expect_snapshot(test_breaks(transform_boxcox(0), limits = c(1, 10)))
  expect_snapshot(test_breaks(transform_modulus(0), c(-10, 10)))
  expect_snapshot(test_breaks(transform_yj(0), c(-10, 10)))
  expect_snapshot(test_breaks("exp", c(-10, 10)))
  expect_snapshot(test_breaks("identity", limits = c(-10, 10)))
  expect_snapshot(test_breaks("log", limits = c(0.1, 1000)))
  expect_snapshot(test_breaks("log10", limits = c(0.1, 1000)))
  expect_snapshot(test_breaks("log2", limits = c(0.5, 32)))
  expect_snapshot(test_breaks("log1p", limits = c(0, 10)))
  expect_snapshot(test_breaks("pseudo_log", limits = c(-10, 10)))
  expect_snapshot(test_breaks("logit", limits = c(0.001, 0.999)))
  expect_snapshot(test_breaks("probit", limits = c(0.001, 0.999)))
  expect_snapshot(test_breaks("reciprocal", limits = c(1, 10)))
  expect_snapshot(test_breaks("reverse", limits = c(-10, 10)))
  expect_snapshot(test_breaks("sqrt", limits = c(0, 10)))
})


# Visual tests ------------------------------------------------------------

test_that("minor breaks draw correctly", {
  df <- data_frame(
    x_num = c(1, 3),
    x_chr = c("a", "b"),
    x_date = as.Date("2012-2-29") + c(0, 100),
    x_log = c(1, 1e4),
    y = c(1, 3)
  )
  theme <- theme_test() +
    theme(
      panel.grid.major = element_line(colour = "grey30", linewidth = 0.5),
      panel.grid.minor = element_line(colour = "grey70")
    )

  p <- ggplot(df, aes(x_num, y)) +
    geom_blank() +
    scale_x_continuous(breaks = 1:3, minor_breaks = c(1.25, 2.75)) +
    scale_y_continuous(breaks = 1:3, minor_breaks = c(1.25, 2.75)) +
    labs(x = NULL, y = NULL) +
    theme
  expect_doppelganger("numeric", p)
  expect_doppelganger("numeric-polar", p + coord_polar())

  expect_doppelganger("numeric-log",
    ggplot(df, aes(x_log, x_log)) +
      scale_x_continuous(transform = transform_log2()) +
      scale_y_log10() +
      labs(x = NULL, y = NULL) +
      theme
  )
  expect_doppelganger("numeric-exp",
    ggplot(df, aes(x_num, x_num)) +
      scale_x_continuous(transform = transform_exp(2)) +
      scale_y_continuous(transform = transform_exp(2)) +
      labs(x = NULL, y = NULL) +
      theme
  )

  expect_doppelganger("character",
    ggplot(df, aes(x_chr, y)) +
      geom_blank() +
      labs(x = NULL, y = NULL) +
      theme
  )

  expect_doppelganger("date",
    ggplot(df, aes(x_date, y)) +
      geom_blank() +
      scale_x_date(
        labels = scales::label_date("%m/%d"),
        breaks = scales::date_breaks("month"),
        minor_breaks = scales::date_breaks("week")
      ) +
      labs(x = NULL, y = NULL) +
      theme
  )
})

test_that("scale breaks can be removed", {
  dat <- data_frame(x = 1:3, y = 1:3)

  expect_doppelganger("no x breaks",
    ggplot(dat, aes(x = x, y = y)) + geom_point() + scale_x_continuous(breaks = NULL)
  )
  expect_doppelganger("no y breaks",
    ggplot(dat, aes(x = x, y = y)) + geom_point() + scale_y_continuous(breaks = NULL)
  )
  expect_doppelganger("no alpha breaks (no legend)",
    ggplot(dat, aes(x = 1, y = y, alpha = x)) + geom_point() + scale_alpha_continuous(breaks = NULL)
  )
  expect_doppelganger("no size breaks (no legend)",
    ggplot(dat, aes(x = 1, y = y, size = x)) + geom_point() + scale_size_continuous(breaks = NULL)
  )
  expect_doppelganger("no fill breaks (no legend)",
    ggplot(dat, aes(x = 1, y = y, fill = x)) + geom_point(shape = 21) + scale_fill_continuous(breaks = NULL)
  )
  expect_doppelganger("no colour breaks (no legend)",
    ggplot(dat, aes(x = 1, y = y, colour = x)) + geom_point() + scale_colour_continuous(breaks = NULL)
  )
})

test_that("functional limits work for continuous scales", {
  limiter <- function(by) {
    function(limits) {
      low <- floor(limits[1] / by) * by
      high <- ceiling(limits[2] / by) * by
      c(low, high)
    }
  }

  expect_doppelganger(
    "functional limits",
    ggplot(mpg, aes(class)) + geom_bar(aes(fill = drv)) + scale_y_continuous(limits = limiter(50))
  )
})

test_that("limits are squished to transformation domain", {
  # Breaks should not be calculated on ranges outside domain #980
  sc1 <- scale_x_sqrt()
  sc2 <- scale_x_sqrt()
  sc3 <- scale_x_reverse(breaks = 1:9) # Test for #4858

  sc1$train(c(0, 10))
  sc2$train(c(-10, 10))
  sc3$train(c(0, -10)) # training expects transformed input

  expect_equal(sc1$get_breaks(), sc2$get_breaks())
  expect_equal(sc2$get_breaks()[1], 0)
  expect_equal(sc3$get_breaks(), -1:-9)
})
