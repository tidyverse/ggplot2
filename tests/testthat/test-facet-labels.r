context("Facet Labels")

get_labels_matrix <- function(plot, ...) {
  data <- ggplot_build(plot)
  layout <- data$layout

  labels <- get_labels_info(layout$facet, layout, ...)
  labeller <- match.fun(layout$facet$params$labeller)

  # Create matrix of labels
  matrix <- lapply(labeller(labels), cbind)
  matrix <- do.call("cbind", matrix)
  matrix
}

get_labels_info <- function(facet, panel, ...) {
  UseMethod("get_labels_info")
}

get_labels_info.FacetGrid <- function(facet, layout, type) {
  if (type == "rows") {
    labels <- unique(layout$layout[names(facet$params$rows)])
    attr(labels, "type") <- "rows"
    attr(labels, "facet") <- "grid"
  } else {
    labels <- unique(layout$layout[names(facet$params$cols)])
    attr(labels, "type") <- "cols"
    attr(labels, "facet") <- "grid"
  }
  labels
}

get_labels_info.FacetWrap <- function(facet, layout) {
  labels <- layout$layout[names(facet$params$facets)]
  attr(labels, "facet") <- "wrap"
  if (!is.null(facet$params$switch) && facet$params$switch == "x") {
    attr(labels, "type") <- "rows"
  } else {
    attr(labels, "type") <- "cols"
  }
  labels
}

test_that("labellers handle facet labels properly", {
  labels <- list(var1 = letters[1:2], var2 = letters[3:4])

  expect_identical(label_value(labels), labels)
  expect_identical(label_value(labels, FALSE), list(c("a, c", "b, d")))

  expect_identical(label_both(labels), list(c("var1: a", "var1: b"), c("var2: c", "var2: d")))
  expect_identical(label_both(labels, FALSE), list(c("var1, var2: a, c", "var1, var2: b, d")))
})

test_that("labellers handle plotmath expressions", {
  labels <- list(var1 = c("alpha", "beta"), var2 = letters[3:4])

  expected_parsed <- list(
    list(expression(alpha), expression(beta)),
    list(expression(c), expression(d))
  )
  expect_identical(label_parsed(labels), expected_parsed)

  expected_parsed_multi <- list(list(
    expression(list(alpha, c)),
    expression(list(beta, d))
  ))
  expect_identical(label_parsed(labels, FALSE), expected_parsed_multi)
})

test_that("label_value() handles factors", {
  labels_chr <- list(var1 = letters[1:2], var2 = letters[3:4])
  labels <- lapply(labels_chr, factor)

  expect_identical(label_value(labels), labels_chr)
})

test_that("labeller() dispatches labellers", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  expected_cyl_both <- cbind(paste("cyl:", c(4, 6, 8)))
  expected_am_both <- cbind(paste("am:", 0:1))

  # Rows and cols dispatch with facet_wrap()
  p1 <- p + facet_wrap(~cyl, labeller = labeller(.rows = label_both))
  p2 <- p + facet_wrap(~cyl, labeller = labeller(.cols = label_both))
  expect_equal(get_labels_matrix(p1), expected_cyl_both)
  expect_equal(get_labels_matrix(p2), expected_cyl_both)

  # facet_wrap() shouldn't get both rows and cols
  p3 <- p + facet_wrap(~cyl, labeller = labeller(
    .cols = label_both, .rows = label_both))
  expect_error(ggplotGrob(p3))

  # facet_grid() can get both rows and cols
  p4 <- p + facet_grid(am ~ cyl, labeller = labeller(
    .cols = label_both, .rows = label_both))
  expect_equal(get_labels_matrix(p4, "rows"), expected_am_both)
  expect_equal(get_labels_matrix(p4, "cols"), expected_cyl_both)

  # Cannot have a specific labeller for a variable which already has a
  # margin-wide labeller
  p5 <- p + facet_wrap(~cyl, labeller = labeller(
    .rows = label_both, cyl = label_value))
  expect_error(ggplotGrob(p5))

  # Variables can be attributed labellers
  p6 <- p + facet_grid(am + cyl ~ ., labeller = labeller(
     am = label_both, cyl = label_both))
  expect_equal(
    get_labels_matrix(p6, "rows"),
    cbind(
      paste("am:", rep(0:1, each = 3)),
      paste("cyl:", rep(c(4, 6, 8), 2))
    )
  )

  # Default labeller is used for other variables
  p7 <- p + facet_grid(am ~ cyl, labeller = labeller(.default = label_both))
  expect_equal(get_labels_matrix(p7, "rows"), expected_am_both)
  expect_equal(get_labels_matrix(p7, "cols"), expected_cyl_both)
})

test_that("as_labeller() deals with non-labellers", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  lookup <- c(`0` = "zero", `1` = "one")

  # Lookup table
  p1 <- p + facet_wrap(~am, labeller = labeller(am = lookup))
  expect_equal(get_labels_matrix(p1), cbind(c("zero", "one")))

  # Non-labeller function taking character vectors
  p2 <- p + facet_wrap(~am, labeller = labeller(am = function(x) paste0(x, "-foo")))
  expect_equal(get_labels_matrix(p2), cbind(c("0-foo", "1-foo")))
})

test_that("old school labellers still work", {
  my_labeller <- function(variable, value) {
    paste0("var = ", as.character(value))
  }

  expect_warning(p <-
    ggplot(mtcars, aes(disp, drat)) +
    geom_point() +
    facet_grid(~cyl, labeller = my_labeller))

  expected_labels <- cbind(paste("var =", c(4, 6, 8)))
  expect_identical(get_labels_matrix(p, "cols"), expected_labels)
})


# Visual test -------------------------------------------------------------

test_that("parsed labels are rendered correctly", {
  df <- data.frame(x = 1, y = 1, f = "alpha ^ beta")

  expect_doppelganger(
    "parsed facet labels",
    ggplot(df, aes(x, y)) +
      labs(x = NULL, y = NULL) +
      facet_wrap(~ f, labeller = label_parsed)
  )
})
