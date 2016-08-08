context("scale_date")


# Visual tests ------------------------------------------------------------

test_that("date scale draws correctly", {
  set.seed(321)
  df <- data.frame(
    dx = seq(as.Date("2012-02-29"), length.out = 100, by = "1 day")[sample(100, 50)],
    price = runif(50)
  )
  df <- df[order(df$dx), ]

  dt <- qplot(dx, price, data = df, geom = "line")
  vdiffr::expect_doppelganger(
    dt,
    "dates along x, default breaks"
  )
  vdiffr::expect_doppelganger(
    dt + scale_x_date(breaks = date_breaks("2 weeks")),
    "scale_x_date(breaks = date_breaks(\"2 weeks\"))"
  )
  vdiffr::expect_doppelganger(
    dt + scale_x_date(date_breaks = "3 weeks"),
    "scale_x_date(breaks = \"3 weeks\")"
  )
  vdiffr::expect_doppelganger(
    dt + scale_x_date(labels = date_format("%m/%d")),
    "scale_x_date(labels = date_format(\"%m/%d\"))"
  )
  vdiffr::expect_doppelganger(
    dt + scale_x_date(labels = date_format("%W"), "week"),
    "scale_x_date(labels = date_format(\"%W\"), \"week\")"
  )

  dt <- qplot(price, dx, data = df, geom = "line")
  vdiffr::expect_doppelganger(
    dt,
    "dates along y, default breaks"
  )
  vdiffr::expect_doppelganger(
    dt + scale_y_date(breaks = date_breaks("2 weeks")),
    "scale_y_date(breaks = date_breaks(\"2 weeks\"))"
  )
  vdiffr::expect_doppelganger(
    dt + scale_y_date(date_breaks = "3 weeks"),
    "scale_y_date(breaks = \"3 weeks\")"
  )
})
