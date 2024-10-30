test_that("as_facets_list() coerces formulas", {
  expect_identical(as_facets_list(~foo), list(quos(), quos(foo = foo)))
  expect_identical(as_facets_list(~foo + bar), list(quos(), quos(foo = foo, bar = bar)))
  expect_identical(as_facets_list(foo ~ bar), list(quos(foo = foo), quos(bar = bar)))

  exp <- list(quos(foo = foo, bar = bar), quos(baz = baz, bam = bam))
  expect_identical(as_facets_list(foo + bar ~ baz + bam), exp)

  exp <- list(quos(`foo()`= foo(), `bar()` = bar()), quos(`baz()` = baz(), `bam()` = bam()))
  expect_identical(as_facets_list(foo() + bar() ~ baz() + bam()), exp)
})

test_that("as_facets_list() coerces strings containing formulas", {
  expect_identical(as_facets_list("foo ~ bar"), as_facets_list(local(foo ~ bar, globalenv())))
})

test_that("as_facets_list() coerces character vectors", {
  foo <- new_quosure(quote(foo), globalenv())
  bar <- new_quosure(quote(bar), globalenv())
  foobar <- as_quosures(list(foo, bar), named = TRUE)

  expect_identical(as_facets_list("foo"), list(foobar[1]))
  expect_identical(as_facets_list(c("foo", "bar")), list(foobar[1], foobar[2]))
  expect_identical(compact_facets(c("foo", "bar")), foobar)
})

test_that("as_facets_list() coerces lists", {
  out <- as_facets_list(list(
    quote(foo),
    c("foo", "bar"),
    NULL
  ))
  exp <- c(
    as_facets_list(quote(foo)),
    list(do.call(base::`c`, as_facets_list(c("foo", "bar")))),
    list(quos_list())
  )
  expect_identical(out, exp)
})

test_that("as_facets_list() coerces quosures objectss", {
  expect_identical(as_facets_list(vars(foo)), list(quos(foo = foo)))
})

test_that("facets reject aes()", {
  expect_snapshot(facet_wrap(aes(foo)), error = TRUE)
  expect_snapshot(facet_grid(aes(foo)), error = TRUE)
})

test_that("compact_facets() returns a quosures object with compacted", {
  expect_identical(compact_facets(vars(foo)), quos(foo = foo))
  expect_identical(compact_facets(~foo + bar), quos(foo = foo, bar = bar))

  f <- function(x) {
    expect_identical(compact_facets(vars(foo, {{ x }}, bar)), quos(foo = foo, bar = bar))
  }

  f(NULL)
  f()
})

test_that("grid_as_facets_list() returns a list of quosures objects with compacted", {
  expect_identical(grid_as_facets_list(vars(foo), NULL), list(rows = quos(foo = foo), cols = quos()))
  expect_identical(grid_as_facets_list(~foo, NULL), list(rows = quos(), cols = quos(foo = foo)))

  f <- function(x) {
    expect_identical(grid_as_facets_list(vars(foo, {{ x }}, bar), NULL), list(rows = quos(foo = foo, bar = bar), cols = quos()))
  }

  f(NULL)
  f()
})

test_that("compact_facets() and grid_as_facets_list() accept empty specs", {
  expect_identical(compact_facets(NULL), quos())
  expect_identical(compact_facets(list()), quos())
  expect_identical(compact_facets(. ~ .), quos())
  expect_identical(compact_facets(list(. ~ .)), quos())
  expect_identical(compact_facets(list(NULL)), quos())

  expect_identical(grid_as_facets_list(list(), NULL), list(rows = quos(), cols = quos()))
  expect_identical(grid_as_facets_list(. ~ ., NULL), list(rows = quos(), cols = quos()))
  expect_identical(grid_as_facets_list(list(. ~ .), NULL), list(rows = quos(), cols = quos()))
  expect_identical(grid_as_facets_list(list(NULL), NULL), list(rows = quos(), cols = quos()))
})

test_that("facets split up the data", {
  df <- data_frame(x = 1:3, y = 3:1, z = letters[1:3])
  p <- ggplot(df, aes(x, y)) + geom_point()

  l1 <- p + facet_wrap(~z)
  l2 <- p + facet_grid(. ~ z)
  l3 <- p + facet_grid(z ~ .)

  d1 <- get_layer_data(l1)
  d2 <- get_layer_data(l2)
  d3 <- get_layer_data(l3)

  expect_equal(d1, d2)
  expect_equal(d1, d3)
  expect_equal(d1$PANEL, factor(1:3))

  # Handle empty layers
  p_empty <- ggplot() + geom_point(aes(x, y), df) + geom_line()
  l4 <- p_empty + facet_wrap(~z)
  l5 <- p_empty + facet_grid(. ~ z)

  d4 <- get_layer_data(l4)
  d5 <- get_layer_data(l5)

  expect_equal(d1, d4)
  expect_equal(d1, d5)
})


test_that("facet_wrap() accepts vars()", {
  df <- data_frame(x = 1:3, y = 3:1, z = letters[1:3])
  p <- ggplot(df, aes(x, y)) + geom_point()

  p1 <- p + facet_wrap(~z)
  p2 <- p + facet_wrap(vars(Z = z), labeller = label_both)

  expect_identical(get_layer_data(p1), get_layer_data(p2))
})

test_that("facet_grid() accepts vars()", {
  grid <- facet_grid(vars(a = foo))
  expect_identical(grid$params$rows, quos(a = foo))

  grid <- facet_grid(vars(a = foo), vars(b = bar))
  expect_identical(grid$params$rows, quos(a = foo))
  expect_identical(grid$params$cols, quos(b = bar))

  grid <- facet_grid(vars(foo), vars(bar))
  expect_identical(grid$params$rows, quos(foo = foo))
  expect_identical(grid$params$cols, quos(bar = bar))

  expect_equal(facet_grid(vars(am, vs))$params, facet_grid(am + vs ~ .)$params)
  expect_equal(facet_grid(vars(am, vs), vars(cyl))$params, facet_grid(am + vs ~ cyl)$params)
  expect_equal(facet_grid(NULL, vars(cyl))$params, facet_grid(. ~ cyl)$params)
  expect_equal(facet_grid(vars(am, vs), TRUE)$params, facet_grid(am + vs ~ ., margins = TRUE)$params)
})

test_that("facet_grid() fails if passed both a formula and a vars()", {
  expect_snapshot_error(facet_grid(~foo, vars()))
})

test_that("can't pass formulas to `cols`", {
  expect_snapshot_error(facet_grid(NULL, ~foo))
})

test_that("can still pass `margins` as second argument", {
  grid <- facet_grid(~foo, TRUE)
  expect_true(grid$params$margins)
})

test_that("vars() accepts optional names", {
  wrap <- facet_wrap(vars(A = a, b))
  expect_named(wrap$params$facets, c("A", "b"))
})

test_that("facet_wrap()/facet_grid() compact the facet spec, and accept empty spec", {
  df <- data_frame(x = 1:3, y = 3:1, z = letters[1:3])
  p <- ggplot(df, aes(x, y)) + geom_point()

  # facet_wrap()
  p_wrap <- p + facet_wrap(vars(NULL))
  d_wrap <- get_layer_data(p_wrap)

  expect_equal(d_wrap$PANEL, factor(c(1L, 1L, 1L)))
  expect_equal(d_wrap$group, structure(c(-1L, -1L, -1L), n = 1L))

  # facet_grid()
  p_grid <- p + facet_grid(vars(NULL))
  d_grid <- get_layer_data(p_grid)

  expect_equal(d_grid$PANEL, factor(c(1L, 1L, 1L)))
  expect_equal(d_grid$group, structure(c(-1L, -1L, -1L), n = 1L))
})


test_that("facets with free scales scale independently", {
  df <- data_frame(x = 1:3, y = 3:1, z = letters[1:3])
  p <- ggplot(df, aes(x, y)) + geom_point()

  # facet_wrap()
  l1 <- p + facet_wrap(~z, scales = "free")
  d1 <- cdata(l1)[[1]]
  expect_true(sd(d1$x) < 1e-10)
  expect_true(sd(d1$y) < 1e-10)

  # RHS of facet_grid()
  l2 <- p + facet_grid(. ~ z, scales = "free")
  d2 <- cdata(l2)[[1]]
  expect_true(sd(d2$x) < 1e-10)
  expect_length(unique(d2$y), 3)

  # LHS of facet_grid()
  l3 <- p + facet_grid(z ~ ., scales = "free")
  d3 <- cdata(l3)[[1]]
  expect_length(unique(d3$x), 3)
  expect_true(sd(d3$y) < 1e-10)
})

test_that("shrink parameter affects scaling", {
  df <- data_frame(x = 1:3, y = 3:1, z = letters[1:3])

  l1 <- ggplot(df, aes(1, y)) + geom_point()
  r1 <- pranges(l1)

  expect_equal(r1$x[[1]], c(1, 1))
  expect_equal(r1$y[[1]], c(1, 3))

  l2 <- ggplot(df, aes(1, y)) + stat_summary(fun = "mean")
  r2 <- pranges(l2)
  expect_equal(r2$y[[1]], c(2, 2))

  l3 <- ggplot(df, aes(1, y)) + stat_summary(fun = "mean") +
    facet_null(shrink = FALSE)
  r3 <- pranges(l3)
  expect_equal(r3$y[[1]], c(1, 3))
})

test_that("facet variables", {
  expect_identical(facet_null()$vars(), character(0))
  expect_identical(facet_wrap(~ a)$vars(), "a")
  expect_identical(facet_grid(a ~ b)$vars(), c("a", "b"))
})

test_that("facet gives clear error if ", {
  df <- data_frame(x = 1)
  expect_snapshot_error(print(ggplot(df, aes(x)) + facet_grid(x ~ x)))
  expect_snapshot_error(print(ggplot(df, aes(x)) %>% facet_grid(. ~ x)))
  expect_snapshot_error(print(ggplot(df, aes(x)) + facet_grid(list(1, 2, 3))))
  expect_snapshot_error(print(ggplot(df, aes(x)) + facet_grid(vars(x), "free")))
})

test_that("facet_grid `axis_labels` argument can be overruled", {

  f <- facet_grid(vars(cyl), axes = "all", axis.labels = "all")
  expect_equal(f$params$axis_labels, list(x = TRUE, y = TRUE))

  f <- facet_grid(vars(cyl), axes = "all", axis.labels = "margins")
  expect_equal(f$params$axis_labels, list(x = FALSE, y = FALSE))

  # Overrule when only drawing at margins
  f <- facet_grid(vars(cyl), axes = "margins", axis.labels = "margins")
  expect_equal(f$params$axis_labels, list(x = TRUE, y = TRUE))

})

test_that("facet_wrap `axis_labels` argument can be overruled", {

  # The folllowing three should all draw axis labels
  f <- facet_wrap(vars(cyl), scales = "fixed", axes = "all", axis.labels = "all")
  expect_equal(f$params$axis_labels, list(x = TRUE, y = TRUE))

  f <- facet_wrap(vars(cyl), scales = "free", axes = "all", axis.labels = "all")
  expect_equal(f$params$axis_labels, list(x = TRUE, y = TRUE))

  f <- facet_wrap(vars(cyl), scales = "fixed", axes = "margins", axis.labels = "all")
  expect_equal(f$params$axis_labels, list(x = TRUE, y = TRUE))

  # The only case when labels shouldn't be drawn is when scales are fixed but
  # the axes are to be drawn
  f <- facet_wrap(vars(cyl), scales = "fixed", axes = "all", axis.labels = "margins")
  expect_equal(f$params$axis_labels, list(x = FALSE, y = FALSE))

  # Should draw labels because scales are free
  f <- facet_wrap(vars(cyl), scales = "free", axes = "all", axis.labels = "margins")
  expect_equal(f$params$axis_labels, list(x = TRUE, y = TRUE))

  # Should draw labels because only drawing at margins
  f <- facet_wrap(vars(cyl), scales = "fixed", axes = "margins", axis.labels = "margins")
  expect_equal(f$params$axis_labels, list(x = TRUE, y = TRUE))

})

test_that("facet_grid `axes` can draw inner axes.", {
  df <- data_frame(
    x = 1:4, y = 1:4,
    fx = c("A", "A", "B", "B"),
    fy = c("c", "d", "c", "d")
  )
  p <- ggplot(df, aes(x, y)) + geom_point()

  case <- ggplotGrob(p + facet_grid(vars(fy), vars(fx), axes = "all"))
  ctrl <- ggplotGrob(p + facet_grid(vars(fy), vars(fx), axes = "margins"))

  # 4 x-axes if all axes should be drawn
  bottom <- case$grobs[grepl("axis-b", case$layout$name)]
  expect_equal(sum(vapply(bottom, inherits, logical(1), "absoluteGrob")), 4)
  # 2 x-axes if drawing at the margins
  bottom <- ctrl$grobs[grepl("axis-b", ctrl$layout$name)]
  expect_equal(sum(vapply(bottom, inherits, logical(1), "absoluteGrob")), 2)

  # Ditto for y-axes
  left <- case$grobs[grepl("axis-l", case$layout$name)]
  expect_equal(sum(vapply(left, inherits, logical(1), "absoluteGrob")), 4)
  left <- ctrl$grobs[grepl("axis-l", ctrl$layout$name)]
  expect_equal(sum(vapply(left, inherits, logical(1), "absoluteGrob")), 2)
})

test_that("facet_wrap `axes` can draw inner axes.", {
  df <- data_frame(
    x = 1, y = 1, facet = LETTERS[1:4]
  )

  p <- ggplot(df, aes(x, y)) + geom_point()

  case <- ggplotGrob(p + facet_wrap(vars(facet), axes = "all"))
  ctrl <- ggplotGrob(p + facet_wrap(vars(facet), axes = "margins"))

  # 4 x-axes if all axes should be drawn
  bottom <- case$grobs[grepl("axis-b", case$layout$name)]
  expect_equal(sum(vapply(bottom, inherits, logical(1), "absoluteGrob")), 4)
  # 2 x-axes if drawing at the margins
  bottom <- ctrl$grobs[grepl("axis-b", ctrl$layout$name)]
  expect_equal(sum(vapply(bottom, inherits, logical(1), "absoluteGrob")), 2)

  # Ditto for y-axes
  left <- case$grobs[grepl("axis-l", case$layout$name)]
  expect_equal(sum(vapply(left, inherits, logical(1), "absoluteGrob")), 4)
  left <- ctrl$grobs[grepl("axis-l", ctrl$layout$name)]
  expect_equal(sum(vapply(left, inherits, logical(1), "absoluteGrob")), 2)
})

test_that("facet_wrap throws deprecation messages", {
  withr::local_options(lifecycle_verbosity = "warning")

  facet <- facet_wrap(vars(year))
  facet$params$dir <- "h"

  lifecycle::expect_deprecated(
    ggplot_build(ggplot(mpg, aes(displ, hwy)) + geom_point() + facet),
    "Internal use of"
  )
})

# Variable combinations ---------------------------------------------------

test_that("zero-length vars in combine_vars() generates zero combinations", {
  df <- data_frame(letter = c("a", "b"))
  expect_equal(nrow(combine_vars(list(df), vars = vars())), 0)
  expect_equal(ncol(combine_vars(list(df), vars = vars())), 0)
})

test_that("at least one layer must contain all facet variables in combine_vars()", {
  df <- data_frame(letter = c("a", "b"))
  expect_silent(combine_vars(list(df), vars = vars(letter = letter)))
  expect_snapshot_error(combine_vars(list(df), vars = vars(letter = number)))
})

test_that("at least one combination must exist in combine_vars()", {
  df <- data_frame(letter = character(0))
  expect_snapshot(
    combine_vars(list(df), vars = vars(letter = letter)),
    error = TRUE
  )
})

test_that("combine_vars() generates the correct combinations", {
  df_one <- data_frame(
    letter = c("a", "b"),
    number = c(1, 2),
    boolean = c(TRUE, FALSE),
    factor = factor(c("level1", "level2"))
  )

  df_all <- expand.grid(
    letter = c("a", "b"),
    number = c(1, 2),
    boolean = c(TRUE, FALSE),
    factor = factor(c("level1", "level2")),
    stringsAsFactors = FALSE
  )
  attr(df_all, "out.attrs") <- NULL

  vars_all <- vars(letter = letter, number =  number, boolean = boolean, factor = factor)

  expect_equal(
    combine_vars(list(df_one), vars = vars_all),
    df_one
  )

  expect_equal(
    combine_vars(list(df_all), vars = vars_all),
    df_all
  )

  # with drop = FALSE the rows are ordered in the opposite order
  # NAs are dropped with drop = FALSE (except for NA factor values);
  # NAs are kept with with drop = TRUE
  # drop keeps all combinations of data, regardless of the combinations in which
  # they appear in the data (in addition to keeping unused factor levels)
  expect_equal(
    combine_vars(list(df_one), vars = vars_all, drop = FALSE),
    df_all[order(df_all$letter, df_all$number, df_all$boolean, df_all$factor), ],
    ignore_attr = TRUE   # do not compare `row.names`
  )

  expect_snapshot_error(
    combine_vars(
      list(data.frame(a = 1:2, b = 2:3), data.frame(a = 1:2, c = 2:3)),
      vars = vars(b=b, c=c)
    )
  )

  expect_snapshot_error(
    combine_vars(
      list(data.frame(a = 1:2), data.frame(b = numeric())),
      vars = vars(b=b)
    )
  )
})

test_that("drop = FALSE in combine_vars() keeps unused factor levels", {
  df <- data_frame(x = factor("a", levels = c("a", "b")))
  expect_equal(
    combine_vars(list(df), vars = vars(x = x), drop = TRUE),
    data_frame(x = factor("a", levels = c("a", "b")))
  )
  expect_equal(
    combine_vars(list(df), vars = vars(x = x), drop = FALSE),
    data_frame(x = factor(c("a", "b"), levels = c("a", "b")))
  )
})

test_that("combine_vars() generates the correct combinations with multiple data frames", {
  df <- expand.grid(letter = c("a", "b"), number = c(1, 2), boolean = c(TRUE, FALSE))

  vars <- vars(letter = letter, number = number)
  expect_identical(
    combine_vars(list(df), vars = vars),
    combine_vars(list(df, df), vars = vars)
  )
  expect_identical(
    combine_vars(list(df), vars = vars),
    combine_vars(list(df, df[character(0)]), vars = vars)
  )
  expect_identical(
    combine_vars(list(df), vars = vars),
    combine_vars(list(df, df["letter"]), vars = vars)
  )
  expect_identical(
    combine_vars(list(df), vars = vars),
    combine_vars(list(df, df[c("letter", "number")]), vars = vars)
  )
})

test_that("eval_facet() is tolerant for missing columns (#2963)", {
  expect_null(eval_facet(quo(2 * x), data_frame(foo = 1), possible_columns = c("x")))
  expect_null(eval_facet(quo(2 * .data$x), data_frame(foo = 1), possible_columns = c("x")))

  # Even if there's the same name of external variable, eval_facet() returns NULL before
  # reaching to the variable
  bar <- 2
  expect_null(eval_facet(quo(2 * bar), data_frame(foo = 1), possible_columns = c("bar")))
  # If there's no same name of columns, the external variable is used
  expect_equal(
    eval_facet(quo(2 * bar), data_frame(foo = 1), possible_columns = c("x")),
    4
  )

  # If the expression contains any non-existent variable, it fails
  expect_snapshot(
    eval_facet(quo(no_such_variable * x), data_frame(foo = 1), possible_columns = c("x")),
    error = TRUE
  )
})

test_that("validate_facets() provide meaningful errors", {
  expect_snapshot_error(validate_facets(aes(var)))
  expect_snapshot_error(validate_facets(ggplot()))
})

test_that("check_layout() throws meaningful errors", {
  expect_snapshot_error(check_layout(mtcars))
})

# Visual tests ------------------------------------------------------------

test_that("facet labels respect both justification and margin arguments", {

  df <- data_frame(
    x = 1:2,
    y = 1:2,
    z = c("a", "aaaaaaabc"),
    g = c("b", "bbbbbbbcd")
  )

  base <- ggplot(df, aes(x, y)) +
    geom_point() +
    facet_grid(g ~ z) +
    theme_test()

  p1 <- base +
    theme(strip.text.x = element_text(hjust = 0, margin = margin(5, 5, 5, 5)),
          strip.text.y = element_text(hjust = 0, margin = margin(5, 5, 5, 5)))

  p2 <- base +
    theme(
      strip.text.x = element_text(
        angle = 90,
        hjust = 0,
        margin = margin(5, 5, 5, 5)
      ),
      strip.text.y = element_text(
        angle = 0,
        hjust = 0,
        margin = margin(5, 5, 5, 5)
      )
    )

  expect_doppelganger("left justified facet labels with margins", p1)
  expect_doppelganger("left justified rotated facet labels with margins", p2)
})

test_that("facet's 'axis_labels' argument correctly omits labels", {

  base <- ggplot(mtcars, aes(mpg, disp)) +
    geom_point() +
    guides(x = "axis", y = "axis", x.sec = "axis", y.sec = "axis")

  expect_doppelganger(
    "facet_grid with omitted inner axis labels",
    base + facet_grid(vars(cyl), vars(vs), axes = "all", axis.labels = "margins")
  )

  expect_doppelganger(
    "facet_wrap with omitted inner axis labels",
    base + facet_wrap(vars(cyl, vs), axes = "all", axis.labels = "margins")
  )
})
