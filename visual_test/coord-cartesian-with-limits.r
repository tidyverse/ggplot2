vcontext("coord-cartesian-with-limits")
## test codes of coord_cartesion with range specification.

p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()

p + coord_cartesian(xlim = c(0, 10), ylim = c(0, 50))
save_vtest("expand range")

p + coord_cartesian(xlim = c(2, 4), ylim = c(20, 40))
save_vtest("contract range")

end_vcontext()
