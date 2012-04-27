vcontext("minor-breaks")
p <- ggplot(NULL, aes(1:3, 1:3)) + geom_point() +
  scale_x_continuous(breaks = 1:3, minor_breaks=c(1.25, 2.75)) +
  scale_y_continuous(breaks = 1:3, minor_breaks=c(1.25, 2.75))

p
save_vtest("manual minor breaks")
p + coord_polar()
save_vtest("manual minor breaks with coord_polar")


set.seed(342)
df <- data.frame(
  date = seq(as.Date("2012-2-29"), len=100, by="1 day")[sample(100, 50)],
  price = runif(50)
)
df <- df[order(df$date), ]
p <- qplot(date, price, data=df, geom="line") +
  scale_x_date(labels = date_format("%m/%d"), breaks = date_breaks("month"), minor_breaks = date_breaks("week"))

p
save_vtest("major breaks: months, minor breaks: weeks")

p + coord_polar()
save_vtest("major breaks: months, minor breaks: weeks, with coord_polar")

ggplot(NULL, aes(letters[1:3], 1:3)) + geom_point()
save_vtest("default breaks")

qplot(1:1e4, 1:1e4) + scale_x_continuous(trans = log2_trans()) + scale_y_log10()
save_vtest("scale_x_continuous(trans = log2_trans()) + scale_y_log10")

qplot(1:5, 1:5) + scale_x_continuous(trans = exp_trans(2)) + scale_y_continuous(trans = exp_trans(2))
save_vtest("scale_x_continuous(trans = exp_trans(2)) + scale_y_continuous(trans = exp_trans(2))")

end_vcontext()
