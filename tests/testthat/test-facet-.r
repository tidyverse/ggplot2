context("Facetting")

test_that("as_facets_list() coerces formulas", {
  expect_identical(as_facets_list(~foo), list(quos(foo = foo)))
  expect_identical(as_facets_list(~foo + bar), list(quos(foo = foo, bar = bar)))

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
  expect_identical(as_facets_list("foo"), as_facets_list(local(~foo, globalenv())))
  expect_identical(as_facets_list(c("foo", "bar")), as_facets_list(local(foo ~ bar, globalenv())))
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

test_that("as_facets_list() errors with empty specs", {
  expect_error(as_facets_list(list()), "at least one variable to facet by")
  expect_error(as_facets_list(. ~ .), "at least one variable to facet by")
  expect_error(as_facets_list(list(. ~ .)), "at least one variable to facet by")
  expect_error(as_facets_list(list(NULL)), "at least one variable to facet by")
})

test_that("as_facets_list() coerces quosure lists", {
  expect_identical(as_facets_list(vars(foo)), list(rlang::quos(foo = foo)))
})


df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])

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
  df <- data.frame(x = 1)
  expect_error(
    print(ggplot(df, aes(x)) + facet_grid(x ~ x)),
    "row or cols, not both"
  )
})


# Visual tests ------------------------------------------------------------

test_that("facet labels respect both justification and margin arguments", {

  df <- data.frame(
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
