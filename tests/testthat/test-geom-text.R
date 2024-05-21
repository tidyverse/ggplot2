test_that("geom_text() checks input", {
  expect_snapshot_error(geom_text(position = "jitter", nudge_x = 0.5))
})

test_that("geom_text() drops missing angles", {

  df <- data_frame0(x = 1, y = 1, label = "A", angle = 0)
  geom <- geom_text()

  expect_silent(
    geom$geom$handle_na(df, geom$geom_params)
  )

  df$angle <- NA
  expect_warning(
    geom$geom$handle_na(df, geom$geom_params),
    "Removed 1 row"
  )
})

test_that("geom_text() accepts mm and pt size units", {
  p <- ggplot(data_frame0(x = 1, y = 1, label = "A"), aes(x, y, label = label))

  grob <- get_layer_grob(p + geom_text(size = 10, size.unit = "mm"))[[1]]
  expect_equal(grob$gp$fontsize, 10 * .pt)

  grob <- get_layer_grob(p + geom_text(size = 10, size.unit = "pt"))[[1]]
  expect_equal(grob$gp$fontsize, 10)
})

test_that("geom_text() rejects exotic units", {
  p <- ggplot(data_frame0(x = 1, y = 1, label = "A"), aes(x, y, label = label))
  expect_error(
    ggplotGrob(p + geom_text(size = 10, size.unit = "npc")),
    "must be one of"
  )
})

# compute_just ------------------------------------------------------------

test_that("vertical and horizontal positions are equivalent", {
  horiz <- compute_just(c("left", "middle", "right"), c(0, 0, 0))
  vert <- compute_just(c("bottom", "center", "top"), c(0, 0, 0))

  expect_equal(horiz, vert)
})

test_that("inward moves text towards center", {
  expect_equal(
    compute_just(c("inward", "inward", "inward"), c(0, 0.5, 1)),
    c(0, 0.5, 1.0)
  )
})

test_that("outward moves text away from center", {
  expect_equal(
    compute_just(c("outward", "outward", "outward"), c(0, 0.5, 1)),
    c(1.0, 0.5, 0)
  )
})

test_that("inward points close to center are centered", {
  expect_equal(
    compute_just(c("inward", "inward", "inward"), c(0.5 - 1e-3, 0.5, 0.5 + 1e-3)),
    c(0.5, 0.5, 0.5)
  )
})

test_that("inward moves text towards center at 90 degrees", {
  expect_equal(
    compute_just(c("inward", "inward", "inward"),
                 c(0, 0.5, 1),
                 c(0, 0.5, 1),
                 c(90, 90, 90)),
    c(0, 0.5, 1.0)
  )
})

test_that("outward moves text away from center at 90 degrees", {
  expect_equal(
    compute_just(c("outward", "outward", "outward"),
                 c(0, 0, 0),
                 c(0, 0.5, 1),
                 c(90, 90, 90)),
    c(1.0, 0.5, 0)
  )
})

test_that("only inward and outward respond to angle", {
  expect_equal(
    compute_just(c("inward", "left", "outward"),
                 c(0, 0, 0),
                 c(0, 0.5, 1),
                 c(90, 90, 90)),
    c(0.0, 0.0, 0.0)
  )
})

test_that("inward moves text towards center at 150 degrees", {
  expect_equal(
    compute_just(c("inward", "inward", "inward"),
                 c(0, 0.5, 1),
                 c(0, 0.5, 1),
                 c(150, 150, 150)),
    c(1.0, 0.5, 0.0)
  )
})

test_that("inward moves text towards center at -90 degrees", {
  expect_equal(
    compute_just(c("inward", "inward", "inward"),
                 c(0, 0.5, 1),
                 c(0, 0.5, 1),
                 c(-90, -90, -90)),
    c(1.0, 0.5, 0.0)
  )
})

test_that("outward moves text away from center at 450 degrees", {
  expect_equal(
    compute_just(c("inward", "inward", "inward"),
                 c(0, 0, 0),
                 c(0, 0.5, 1),
                 c(450, 450, 450)),
    c(0.0, 0.5, 1.0)
  )
})
