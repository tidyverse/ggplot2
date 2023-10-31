
# Visual tests ------------------------------------------------------------

test_that("date scale draws correctly", {
  # datetime labels are locale dependent
  withr::local_locale(c(LC_TIME = "C"))

  set.seed(321)
  df <- data_frame(
    dx = seq(as.Date("2012-02-29"), length.out = 100, by = "1 day")[sample(100, 50)],
    price = runif(50)
  )
  df <- df[order(df$dx), ]

  dt <- ggplot(df, aes(dx, price)) + geom_line()
  expect_doppelganger("dates along x, default breaks",
    dt
  )
  expect_doppelganger("scale_x_date(breaks = breaks_width(\"2 weeks\"))",
    dt + scale_x_date(breaks = breaks_width("2 weeks"))
  )
  expect_doppelganger("scale_x_date(breaks = \"3 weeks\")",
    dt + scale_x_date(date_breaks = "3 weeks")
  )
  expect_doppelganger("scale_x_date(labels = label_date(\"%m/%d\"))",
    dt + scale_x_date(labels = label_date("%m/%d"))
  )
  expect_doppelganger("scale_x_date(labels = label_date(\"%W\"), \"week\")",
    dt + scale_x_date(labels = label_date("%W"), "week")
  )

  dt <- ggplot(df, aes(price, dx)) + geom_line()
  expect_doppelganger("dates along y, default breaks", dt)
  expect_doppelganger("scale_y_date(breaks = breaks_width(\"2 weeks\"))",
    dt + scale_y_date(breaks = breaks_width("2 weeks"))
  )
  expect_doppelganger("scale_y_date(breaks = \"3 weeks\")",
    dt + scale_y_date(date_breaks = "3 weeks")
  )
})
