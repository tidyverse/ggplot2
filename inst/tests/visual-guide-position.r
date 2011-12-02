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
