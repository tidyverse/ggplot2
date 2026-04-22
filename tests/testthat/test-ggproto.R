test_that(".DollarNames retrieves inherited methods", {
  A <- ggproto("A", NULL, a = 1)
  B <- ggproto("B", A, b = 2)

  expect_equal(.DollarNames(B), c("b", "a"))
})

test_that("construction checks input", {
  expect_snapshot_error(ggproto("Test", NULL, function(self, a) a))
  expect_snapshot_error(ggproto("Test", NULL, a <- function(self, a) a))
  expect_snapshot_error(ggproto("Test", mtcars, a = function(self, a) a))
  # Duplicate names
  expect_snapshot_error(ggproto("Test", NULL, foo = 20, foo = "A"))
})

test_that("all ggproto methods start with `{` (#6459)", {

  ggprotos <- Filter(
    function(x) inherits(x, "ggproto"),
    mget(ls("package:ggplot2"), asNamespace("ggplot2"), ifnotfound = list(NULL))
  )

  lacks_brackets <- function(method) {
    if (!inherits(method, "ggproto_method")) {
      return(FALSE)
    }
    body <- as.list(body(environment(method)$f))
    if (length(body) == 0 || body[[1]] != quote(`{`)) {
      return(TRUE)
    }
    return(FALSE)
  }

  report_no_bracket <- function(ggproto_class) {
    unlist(lapply(
      ls(envir = ggproto_class),
      function(method) {
        has_brackets <- !lacks_brackets(ggproto_class[[method]])
        if (has_brackets) {
          return(character())
        }
        return(method)
      }
    ))
  }

  # Test to make sure we're testing correctly
  ctrl <- list(
    foo = ggproto("Dummy", dummy = function(x) x + 10),
    bar = ggproto("Dummy", dummy = function(x) {x + 10})
  )
  ctrl <- lapply(ctrl, report_no_bracket)
  expect_equal(ctrl, list(foo = "dummy", bar = character()))

  # Actual relevant test
  failures <- lapply(ggprotos, report_no_bracket)
  failures <- failures[lengths(failures) > 0]
  expect_equal(names(failures), character())
})

test_that("ggproto objects print well", {
  Foo <- ggproto(
    "Foo",
    env = empty_env(),
    num = 12,
    method = function(x) print(x),
    empty = NULL,
    theme = theme()
  )

  expect_snapshot(print(Foo))
})
