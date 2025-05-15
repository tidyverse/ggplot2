test_that(".DollarNames retrieves inherited methods", {
  A <- ggproto("A", NULL, a = 1)
  B <- ggproto("B", A, b = 2)

  expect_equal(.DollarNames(B), c("b", "a"))
})

test_that("construction checks input", {
  expect_snapshot_error(ggproto("Test", NULL, function(self, a) a))
  expect_snapshot_error(ggproto("Test", NULL, a <- function(self, a) a))
  expect_snapshot_error(ggproto("Test", mtcars, a = function(self, a) a))
})

test_that("all ggproto methods start with `{` (#6459)", {

  ggprotos <- Filter(
    function(x) inherits(x, "ggproto"),
    mget(ls("package:ggplot2"), asNamespace("ggplot2"), ifnotfound = list(NULL))
  )

  method_nobrackets <- lapply(ggprotos, function(x) {
    Filter(
      function(m) inherits(x[[m]], "ggproto_method") && {
        b <- as.list(body(get(m, x)))
        length(b) == 0 || b[[1]] != quote(`{`)
      },
      ls(envir = x)
    )
  })

  expect_length(Filter(length, method_nobrackets), 0)

})
