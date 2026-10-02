test_that("fortify.default proves a helpful error with mapping class", {
  expect_snapshot_error(ggplot(aes(x = x)))
})

test_that("fortify.default can handle healthy data-frame-like objects", {
  X <- 1:10
  Y <- runif(length(X))
  Z <- rpois(length(X), 0.8)

  # Not even data-frame-like

  expect_snapshot(fortify(X), error = TRUE)
  expect_snapshot(fortify(array(1:60, 5:3)), error = TRUE)

  # Unhealthy data-frame-like (matrix with no colnames)

  expect_snapshot(fortify(cbind(X, Y, Z, deparse.level=0)), error = TRUE)

  # Healthy data-frame-like (matrix with colnames)

  expect_identical(fortify(cbind(X, Y, Z)), as.data.frame(cbind(X, Y, Z)))

  # Some weird data-frame-like thing that fortify.default() considers
  # healthy (dim(), colnames(), and as.data.frame() behaviors are aligned)

  object <- setNames(Y, head(letters, length(Y)))
  class(object) <- "foo"

  dim.foo <- function(x) c(length(x), 2L)
  registerS3method("dim", "foo", dim.foo)

  dimnames.foo <- function(x) list(format(seq_along(x)), c("key", "value"))
  registerS3method("dimnames", "foo", dimnames.foo)

  as.data.frame.foo <- function(x, row.names = NULL, ...) {
    key <- if (is.null(names(x))) rownames(x) else names(x)
    data.frame(key=key, value=unname(unclass(x)))
  }
  registerS3method("as.data.frame", "foo", as.data.frame.foo)

  expect_identical(fortify(object), data.frame(key=names(object), value=Y))

  # Rejected by fortify.default() because of unhealthy dim() behavior

  skip_if(getRversion() <= "4.4.0")

  dim.foo <- function(x) stop("oops!")
  registerS3method("dim", "foo", dim.foo)
  expect_snapshot(fortify(object), error = TRUE)

  dim.foo <- function(x) c(length(x), 2)
  registerS3method("dim", "foo", dim.foo)
  expect_snapshot(fortify(object), error = TRUE)

  dim.foo <- function(x) 5:2
  registerS3method("dim", "foo", dim.foo)
  expect_snapshot(fortify(object), error = TRUE)

  dim.foo <- function(x) c(length(x), NA_integer_)
  registerS3method("dim", "foo", dim.foo)
  expect_snapshot(fortify(object), error = TRUE)

  dim.foo <- function(x) c(length(x), -5L)
  registerS3method("dim", "foo", dim.foo)
  expect_snapshot(fortify(object), error = TRUE)

  # Repair dim(<foo>)

  dim.foo <- function(x) c(length(x), 2L)
  registerS3method("dim", "foo", dim.foo)

  # Rejected by fortify.default() because of unhealthy colnames() behavior

  dimnames.foo <- function(x) list()  # this breaks colnames(<foo>)
  registerS3method("dimnames", "foo", dimnames.foo)
  expect_snapshot(fortify(object), error = TRUE)

  dimnames.foo <- function(x) list(format(seq_along(x)), toupper)
  registerS3method("dimnames", "foo", dimnames.foo)
  expect_snapshot(fortify(object), error = TRUE)

  # Rejected by fortify.default() because behaviors of dim() and colnames()
  # don't align

  dimnames.foo <- function(x) list(NULL, c("X1", "X2", "X3"))
  registerS3method("dimnames", "foo", dimnames.foo)
  expect_snapshot(fortify(object), error = TRUE)

  # Repair colnames(<foo>)

  dimnames.foo <- function(x) list(format(seq_along(x)), c("key", "value"))
  registerS3method("dimnames", "foo", dimnames.foo)

  # Rejected by fortify.default() because of unhealthy as.data.frame() behavior

  as.data.frame.foo <- function(x, row.names = NULL, ...) stop("oops!")
  registerS3method("as.data.frame", "foo", as.data.frame.foo)
  expect_snapshot(fortify(object), error = TRUE)

  as.data.frame.foo <- function(x, row.names = NULL, ...) "whatever"
  registerS3method("as.data.frame", "foo", as.data.frame.foo)
  expect_snapshot(fortify(object), error = TRUE)

  as.data.frame.foo <- function(x, row.names = NULL, ...) data.frame()
  registerS3method("as.data.frame", "foo", as.data.frame.foo)

  expect_snapshot(fortify(object), error = TRUE)

  as.data.frame.foo <- function(x, row.names = NULL, ...) {
    key <- if (is.null(names(x))) rownames(x) else names(x)
    data.frame(oops=key, value=unname(unclass(x)))
  }
  registerS3method("as.data.frame", "foo", as.data.frame.foo)
  expect_snapshot(fortify(object), error = TRUE)
})
