vcontext("coord-cartesian-with-limits")
## test codes of coord_cartesion with range specification.

p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()

p + coord_cartesian(xlim = c(0, 10), ylim = c(0, 50), wise = TRUE)
save_vtest("expand range, wise=TRUE")

p + coord_cartesian(xlim = c(0, 10), ylim = c(0, 50), wise = FALSE)
save_vtest("expand range, wise=FALSE")

p + coord_cartesian(xlim = c(2, 4), ylim = c(20, 40), wise = TRUE)
save_vtest("contract range, wise=TRUE")

p + coord_cartesian(xlim = c(2, 4), ylim = c(20, 40), wise = FALSE)
save_vtest("contract range, wise=FALSE")

end_vcontext()
