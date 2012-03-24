p1 <- ggplot(mtcars, aes(mpg, disp, colour=cyl)) + geom_point() + opts(title = "title of plot", axis.text.x = theme_text(angle = 90)) +
  scale_x_continuous(breaks = mean(mtcars$mpg), labels = "very very long long axis label") +
  scale_y_continuous(breaks = mean(mtcars$disp), labels = "very very long long axis label")

p1 + opts(legend.position = "left")
p1 + opts(legend.position = "bottom")
p1 + opts(legend.position = "right")
p1 + opts(legend.position = "top")

p1 + facet_grid(am~vs) + opts(legend.position = "left")
p1 + facet_grid(am~vs) + opts(legend.position = "bottom")
p1 + facet_grid(am~vs) + opts(legend.position = "right")
p1 + facet_grid(am~vs) + opts(legend.position = "top")

p1 + facet_wrap(am~vs) + opts(legend.position = "left")
p1 + facet_wrap(am~vs) + opts(legend.position = "bottom")
p1 + facet_wrap(am~vs) + opts(legend.position = "right")
p1 + facet_wrap(am~vs) + opts(legend.position = "top")

# padding
dat <- data.frame(x=LETTERS[1:3], y=1)
ggplot(dat, aes(x=x, y=y, fill=x, colour = 1:3)) + geom_bar() + opts(legend.background = theme_rect()) + guides(color = "colorbar")
