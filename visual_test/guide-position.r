vcontext("guide-position")

p1 <- ggplot(mtcars, aes(mpg, disp, colour=cyl)) + geom_point() + labs(title = "title of plot") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks = mean(mtcars$mpg), labels = "very very long long axis label") +
  scale_y_continuous(breaks = mean(mtcars$disp), labels = "very very long long axis label")

p1 + theme(legend.position = "left")
save_vtest("legend on left")
p1 + theme(legend.position = "bottom")
save_vtest("legend on bottom")
p1 + theme(legend.position = "right")
save_vtest("legend on right")
p1 + theme(legend.position = "top")
save_vtest("legend on top")

p1 + facet_grid(am~vs) + theme(legend.position = "left")
save_vtest("facet_grid, legend on left")
p1 + facet_grid(am~vs) + theme(legend.position = "bottom")
save_vtest("facet_grid, legend on bottom")
p1 + facet_grid(am~vs) + theme(legend.position = "right")
save_vtest("facet_grid, legend on right")
p1 + facet_grid(am~vs) + theme(legend.position = "top")
save_vtest("facet_grid, legend on top")

p1 + facet_wrap(am~vs) + theme(legend.position = "left")
save_vtest("facet_wrap, legend on left")
p1 + facet_wrap(am~vs) + theme(legend.position = "bottom")
save_vtest("facet_wrap, legend on bottom")
p1 + facet_wrap(am~vs) + theme(legend.position = "right")
save_vtest("facet_wrap, legend on right")
p1 + facet_wrap(am~vs) + theme(legend.position = "top")
save_vtest("facet_wrap, legend on top")

# padding
dat <- data.frame(x=LETTERS[1:3], y=1)
p2 <- ggplot(dat, aes(x=x, y=y, fill=x, colour = 1:3)) + geom_bar() +
  theme(legend.background = element_rect(colour="black")) + guides(color = "colorbar")
p2
save_vtest("padding in legend box")


# Placement of legend inside
p2 + theme(legend.position=c(.5, .5))
save_vtest("legend inside plot, centered")

p2 + theme(legend.justification=c(0,0), legend.position=c(0,0))
save_vtest("legend inside plot, bottom left")

p2 + theme(legend.justification=c(1,1), legend.position=c(1,1))
save_vtest("legend inside plot, top right")

p2 + theme(legend.justification=c(0,0), legend.position=c(.5,.5))
save_vtest("legend inside plot, bottom left of legend at center")



end_vcontext()