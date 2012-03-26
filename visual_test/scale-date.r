vcontext("scale-date")

set.seed(321)
df <- data.frame(
  dx = seq(Sys.Date(), len=100, by="1 day")[sample(100, 50)],
  price = runif(50)
)
df <- df[order(df$dx), ]

dt <- qplot(dx, price, data=df, geom="line")
dt
save_vtest("dates along x, default breaks")
dt + scale_x_date(breaks = date_breaks("2 weeks"))
save_vtest("scale_x_date(breaks = date_breaks(\"2 weeks\"))")
dt + scale_x_date(breaks = "3 weeks")
save_vtest("scale_x_date(breaks = date_breaks(\"3 weeks\"))")

dt + scale_x_date(labels = date_format("%m/%d"))
save_vtest("scale_x_date(labels = date_format(\"%m/%d\"))")

dt + scale_x_date(labels = date_format("%W"), "week")
save_vtest("scale_x_date(labels = date_format(\"%W\"), \"week\")")

dt <- qplot(price, dx, data=df, geom="line")
dt
save_vtest("dates along y, default breaks")
dt + scale_y_date(breaks = date_breaks("2 weeks"))
save_vtest("scale_y_date(breaks = date_breaks(\"2 weeks\"))")
dt + scale_y_date(breaks = "3 weeks")
save_vtest("scale_y_date(breaks = date_breaks(\"3 weeks\"))")

end_vcontext()