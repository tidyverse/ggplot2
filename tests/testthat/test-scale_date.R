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
  expect_doppelganger("dates along x, default breaks",
    dt
  )
  expect_doppelganger("scale_x_date(breaks = date_breaks(\"2 weeks\"))",
    dt + scale_x_date(breaks = date_breaks("2 weeks"))
  )
  expect_doppelganger("scale_x_date(breaks = \"3 weeks\")",
    dt + scale_x_date(date_breaks = "3 weeks")
  )
  expect_doppelganger("scale_x_date(labels = date_format(\"%m/%d\"))",
    dt + scale_x_date(labels = date_format("%m/%d"))
  )
  expect_doppelganger("scale_x_date(labels = date_format(\"%W\"), \"week\")",
    dt + scale_x_date(labels = date_format("%W"), "week")
  )

  dt <- qplot(price, dx, data = df, geom = "line")
  expect_doppelganger("dates along y, default breaks", dt)
  expect_doppelganger("scale_y_date(breaks = date_breaks(\"2 weeks\"))",
    dt + scale_y_date(breaks = date_breaks("2 weeks"))
  )
  expect_doppelganger("scale_y_date(breaks = \"3 weeks\")",
    dt + scale_y_date(date_breaks = "3 weeks")
  )
})
