context("Facetting")

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
  expect_identical(wrap_as_facets_list(c("foo", "bar")), foobar)
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
  expect_error(facet_wrap(aes(foo)), "Please use `vars()` to supply facet variables", fixed = TRUE)
  expect_error(facet_grid(aes(foo)), "Please use `vars()` to supply facet variables", fixed = TRUE)
})

test_that("wrap_as_facets_list() returns a quosures object with compacted", {
  expect_identical(wrap_as_facets_list(vars(foo)), quos(foo = foo))
  expect_identical(wrap_as_facets_list(~foo + bar), quos(foo = foo, bar = bar))
  expect_identical(wrap_as_facets_list(vars(foo, NULL, bar)), quos(foo = foo, bar = bar))
})

test_that("grid_as_facets_list() returns a list of quosures objects with compacted", {
  expect_identical(grid_as_facets_list(vars(foo), NULL), list(rows = quos(foo = foo), cols = quos()))
  expect_identical(grid_as_facets_list(~foo, NULL), list(rows = quos(), cols = quos(foo = foo)))
  expect_identical(grid_as_facets_list(vars(foo, NULL, bar), NULL), list(rows = quos(foo = foo, bar = bar), cols = quos()))
})

test_that("wrap_as_facets_list() and grid_as_facets_list() accept empty specs", {
  expect_identical(wrap_as_facets_list(NULL), quos())
  expect_identical(wrap_as_facets_list(list()), quos())
  expect_identical(wrap_as_facets_list(. ~ .), quos())
  expect_identical(wrap_as_facets_list(list(. ~ .)), quos())
  expect_identical(wrap_as_facets_list(list(NULL)), quos())

  expect_identical(grid_as_facets_list(list(), NULL), list(rows = quos(), cols = quos()))
  expect_identical(grid_as_facets_list(. ~ ., NULL), list(rows = quos(), cols = quos()))
  expect_identical(grid_as_facets_list(list(. ~ .), NULL), list(rows = quos(), cols = quos()))
  expect_identical(grid_as_facets_list(list(NULL), NULL), list(rows = quos(), cols = quos()))
})

df <- data_frame(x = 1:3, y = 3:1, z = letters[1:3])

test_that("facets split up the data", {
  l1 <- ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~z)
  l2 <- ggplot(df, aes(x, y)) + geom_point() + facet_grid(. ~ z)
  l3 <- ggplot(df, aes(x, y)) + geom_point() + facet_grid(z ~ .)

  d1 <- layer_data(l1)
  d2 <- layer_data(l2)
  d3 <- layer_data(l3)

  expect_equal(d1, d2)
  expect_equal(d1, d3)
  expect_equal(d1$PANEL, factor(1:3))
})

test_that("facet_wrap() accepts vars()", {
  p <- ggplot(df, aes(x, y)) + geom_point()
  p2 <- p + facet_wrap(vars(z))

  p1 <- p + facet_wrap(~z)
  p2 <- p + facet_wrap(vars(Z = z), labeller = label_both)

  expect_identical(layer_data(p1), layer_data(p2))
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

  expect_equal(facet_grid(vars(am, vs)), facet_grid(am + vs ~ .))
  expect_equal(facet_grid(vars(am, vs), vars(cyl)), facet_grid(am + vs ~ cyl))
  expect_equal(facet_grid(NULL, vars(cyl)), facet_grid(. ~ cyl))
  expect_equal(facet_grid(vars(am, vs), TRUE), facet_grid(am + vs ~ ., margins = TRUE))
})

test_that("facet_grid() fails if passed both a formula and a vars()", {
  expect_error(facet_grid(~foo, vars()), "`rows` must be `NULL` or a `vars\\(\\)` list if")
})

test_that("can't pass formulas to `cols`", {
  expect_error(facet_grid(NULL, ~foo), "`cols` must be `NULL` or a `vars\\(\\)`")
})

test_that("can still pass `margins` as second argument", {
  grid <- facet_grid(~foo, TRUE)
  expect_true(grid$params$margins)
})

test_that("vars() accepts optional names", {
  wrap <- facet_wrap(vars(A = a, b))
  expect_named(wrap$params$facets, c("A", "b"))
})

test_that("facets_wrap() compacts the facet spec and accept empty spec", {
  p <- ggplot(df, aes(x, y)) + geom_point() + facet_wrap(vars(NULL))
  d <- layer_data(p)

  expect_equal(d$PANEL, c(1L, 1L, 1L))
  expect_equal(d$group, c(-1L, -1L, -1L))
})

test_that("facets_grid() compacts the facet spec and accept empty spec", {
  p <- ggplot(df, aes(x, y)) + geom_point() + facet_grid(vars(NULL))
  d <- layer_data(p)

  expect_equal(d$PANEL, c(1L, 1L, 1L))
  expect_equal(d$group, c(-1L, -1L, -1L))
})


test_that("facets with free scales scale independently", {
  l1 <- ggplot(df, aes(x, y)) + geom_point() +
    facet_wrap(~z, scales = "free")
  d1 <- cdata(l1)[[1]]
  expect_true(sd(d1$x) < 1e-10)
  expect_true(sd(d1$y) < 1e-10)

  l2 <- ggplot(df, aes(x, y)) + geom_point() +
    facet_grid(. ~ z, scales = "free")
  d2 <- cdata(l2)[[1]]
  expect_true(sd(d2$x) < 1e-10)
  expect_equal(length(unique(d2$y)), 3)

  l3 <- ggplot(df, aes(x, y)) + geom_point() +
    facet_grid(z ~ ., scales = "free")
  d3 <- cdata(l3)[[1]]
  expect_equal(length(unique(d3$x)), 3)
  expect_true(sd(d3$y) < 1e-10)
})

test_that("shrink parameter affects scaling", {
  l1 <- ggplot(df, aes(1, y)) + geom_point()
  r1 <- pranges(l1)

  expect_equal(r1$x[[1]], c(1, 1))
  expect_equal(r1$y[[1]], c(1, 3))

  l2 <- ggplot(df, aes(1, y)) + stat_summary(fun.y = "mean")
  r2 <- pranges(l2)
  expect_equal(r2$y[[1]], c(2, 2))

  l3 <- ggplot(df, aes(1, y)) + stat_summary(fun.y = "mean") +
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
  expect_error(
    print(ggplot(df, aes(x)) + facet_grid(x ~ x)),
    "row or cols, not both"
  )
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
