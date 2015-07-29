context("Facet Labels")

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
