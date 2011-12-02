library(testthat)
library_if_available(ggplot2)

mtcar_mean <- function(vars) {
  ddply(mtcars, vars, summarise, mpg = mean(mpg), wt = mean(wt))
}

vs <- mtcar_mean(c("vs", "cyl"))
am <- mtcar_mean(c("am", "cyl"))
vsam <- mtcar_mean(c("vs", "am", "cyl"))

plot_data <- function(plot) {
  ggplot_build(plot)$data
}

base <- qplot(mpg, wt, data = mtcars, colour = cyl)

base
base + facet_grid(cyl ~ .)
base + facet_grid(. ~ cyl)
base + facet_grid(vs ~ am)
base + facet_grid(vs + am ~ . )
base + facet_grid(. ~ vs + am)
base + facet_grid(cyl ~ vs + am)

base_vsam <- base + facet_grid(vs ~ am)

base_vsam
base_vsam + geom_point(data = vs, size = 4)
base_vsam + geom_point(data = am, size = 4)
base_vsam + geom_point(data = vsam, size = 4)

base_vsam + geom_point(data = subset(vs, vs == 1), size = 4)
base_vsam + geom_point(data = subset(am, am == 1), size = 4)
base_vsam + geom_point(data = subset(vsam, am == 1 & vs == 1), size = 4)