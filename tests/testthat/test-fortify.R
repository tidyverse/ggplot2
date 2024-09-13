test_that("spatial polygons have correct ordering", {
  suppressPackageStartupMessages({
    skip_if_not_installed("sp")
  })


  make_square <- function(x = 0, y = 0, height = 1, width = 1){
    delx <- width/2
    dely <- height/2
    sp::Polygon(matrix(c(x + delx, x - delx,x - delx,x + delx,x + delx ,
        y - dely,y - dely,y + dely,y + dely,y - dely), ncol = 2))
  }

  make_hole <- function(x = 0, y = 0, height = 0.5, width = 0.5){
    p <- make_square(x = x, y = y, height = height, width = width)
    p@hole <- TRUE
    p
  }

  fake_data <- data_frame(ids = 1:5, region = c(1,1,2,3,4))
  rownames(fake_data) <- 1:5
  polys <- list(sp::Polygons(list(make_square(), make_hole()), 1),
                sp::Polygons(list(make_square(1,0), make_square(2, 0)), 2),
                sp::Polygons(list(make_square(1,1)), 3),
                sp::Polygons(list(make_square(0,1)), 4),
                sp::Polygons(list(make_square(0,3)), 5))

  polys_sp <- sp::SpatialPolygons(polys)
  fake_sp <- sp::SpatialPolygonsDataFrame(polys_sp, fake_data)

  # now reorder regions
  polys2 <- rev(polys)
  polys2_sp <- sp::SpatialPolygons(polys2)
  fake_sp2 <- sp::SpatialPolygonsDataFrame(polys2_sp, fake_data)
  lifecycle::expect_deprecated(
    # supressing: Regions defined for each Polygons
    expected <- suppressMessages(fortify(fake_sp2))
  )
  expected <- expected[order(expected$id, expected$order), ]

  lifecycle::expect_deprecated(
    # supressing: Regions defined for each Polygons
    actual <- suppressMessages(fortify(fake_sp))
  )

  # the levels are different, so these columns need to be converted to character to compare
  expected$group <- as.character(expected$group)
  actual$group <- as.character(actual$group)

  # Use expect_equal(ignore_attr = TRUE) to ignore rownames
  expect_equal(actual, expected, ignore_attr = TRUE)

  lifecycle::expect_deprecated(
    # fortify() with region is defunct due to maptools' retirement
    lifecycle::expect_defunct(fortify(fake_sp, region = "foo"))
  )
})

test_that("fortify.default proves a helpful error with class uneval", {
  expect_snapshot_error(ggplot(aes(x = x)))
})

test_that("fortify.default can handle healthy data-frame-like objects", {
  X <- 1:10
  Y <- runif(length(X))
  Z <- rpois(length(X), 0.8)

  # Not even data-frame-like

  expect_error(fortify(X))
  expect_error(fortify(array(1:60, 5:3)))

  # Unhealthy data-frame-like (matrix with no colnames)

  expect_error(fortify(cbind(X, Y, Z, deparse.level=0)))

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

  dim.foo <- function(x) stop("oops!")
  registerS3method("dim", "foo", dim.foo)
  expect_error(fortify(object))

  dim.foo <- function(x) c(length(x), 2)
  registerS3method("dim", "foo", dim.foo)
  expect_error(fortify(object))

  dim.foo <- function(x) 5:2
  registerS3method("dim", "foo", dim.foo)
  expect_error(fortify(object))

  dim.foo <- function(x) c(length(x), NA_integer_)
  registerS3method("dim", "foo", dim.foo)
  expect_error(fortify(object))

  dim.foo <- function(x) c(length(x), -5L)
  registerS3method("dim", "foo", dim.foo)
  expect_error(fortify(object))

  # Repair dim(<foo>)

  dim.foo <- function(x) c(length(x), 2L)
  registerS3method("dim", "foo", dim.foo)

  # Rejected by fortify.default() because of unhealthy colnames() behavior

  dimnames.foo <- function(x) list()  # this breaks colnames(<foo>)
  registerS3method("dimnames", "foo", dimnames.foo)
  expect_error(fortify(object))

  dimnames.foo <- function(x) list(format(seq_along(x)), toupper)
  registerS3method("dimnames", "foo", dimnames.foo)
  expect_error(fortify(object))

  # Rejected by fortify.default() because behaviors of dim() and colnames()
  # don't align

  dimnames.foo <- function(x) list(NULL, c("X1", "X2", "X3"))
  registerS3method("dimnames", "foo", dimnames.foo)
  expect_error(fortify(object))

  # Repair colnames(<foo>)

  dimnames.foo <- function(x) list(format(seq_along(x)), c("key", "value"))
  registerS3method("dimnames", "foo", dimnames.foo)

  # Rejected by fortify.default() because of unhealthy as.data.frame() behavior

  as.data.frame.foo <- function(x, row.names = NULL, ...) stop("oops!")
  registerS3method("as.data.frame", "foo", as.data.frame.foo)
  expect_error(fortify(object))

  as.data.frame.foo <- function(x, row.names = NULL, ...) "whatever"
  registerS3method("as.data.frame", "foo", as.data.frame.foo)
  expect_error(fortify(object))

  as.data.frame.foo <- function(x, row.names = NULL, ...) data.frame()
  registerS3method("as.data.frame", "foo", as.data.frame.foo)
  expect_error(fortify(object))

  as.data.frame.foo <- function(x, row.names = NULL, ...) {
    key <- if (is.null(names(x))) rownames(x) else names(x)
    data.frame(oops=key, value=unname(unclass(x)))
  }
  registerS3method("as.data.frame", "foo", as.data.frame.foo)
  expect_error(fortify(object))
})
