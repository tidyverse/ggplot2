df <- data.frame(
  dx = seq(Sys.Date(), len=100, by="1 day")[sample(100, 50)],
  price = runif(50)
)
df <- df[order(df$dx), ]

dt <- qplot(dx, price, data=df, geom="line")
dt + scale_x_date(breaks = date_breaks("2 weeks"))
dt + scale_x_date(breaks = "3 weeks")

dt + scale_x_date(labels = date_format("%m/%d"))
dt + scale_x_date(labels = date_format("%W"), "week")

dt <- qplot(price, dx, data=df, geom="line")
dt + scale_y_date(breaks = date_breaks("2 weeks"))
dt + scale_y_date(breaks = "3 weeks")
