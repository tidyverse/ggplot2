\name{opts}
\alias{opts}
\title{Set options/theme elements for a single plot}
\usage{
  opts(...)
}
\arguments{
  \item{...}{a list of element name, element pairings that
  modify the existing theme.}
}
\description{
  Use this function if you want to modify a few theme
  settings for a single plot.
}
\section{Theme elements}{
  The individual theme elements are:

  \tabular{ll}{ axis.line \tab line along axis \cr
  axis.text.x \tab x axis label \cr axis.text.y \tab y axis
  label \cr axis.ticks \tab axis tick marks \cr
  axis.ticks.length \tab tick mark length \cr
  axis.ticks.margin \tab tick mark margin spacing \cr
  axis.title.x \tab horizontal tick labels \cr axis.title.y
  \tab vertical tick labels \cr legend.background \tab
  background of legend \cr legend.margin \tab extra space
  added around legend (both width or height depending on
  orientation of legend) \cr legend.key \tab background
  underneath legend keys \cr legend.key.size \tab key
  background size \cr legend.key.height \tab key background
  height \cr legend.key.width \tab key background width \cr
  legend.text \tab legend labels \cr legend.text.align \tab
  alignment of legend labels \cr legend.title \tab legend
  name \cr legend.title.align \tab alignment of legend
  title \cr legend.position \tab A string or numeric vector
  specifying the position of guides (legends).  Possible
  values are: "left", "right", "bottom", "top", and
  two-element numeric vector. \cr legend.justification \tab
  alignment of legend \cr legend.direction \tab horizontal
  or vertical \cr legend.box \tab A string specifying the
  direction of multiple guides. Possible string values are:
  "horizontal" and "vertical". \cr panel.background \tab
  background of panel \cr panel.border \tab border around
  panel \cr panel.margin \tab margin around facet panels
  \cr panel.grid.major \tab major grid lines \cr
  panel.grid.minor \tab minor grid lines \cr
  plot.background \tab background of the entire plot \cr
  plot.title \tab plot title \cr plot.margin \tab plot
  margins \cr strip.background \tab background of facet
  labels \cr strip.text.x \tab text for horizontal strips
  \cr strip.text.y \tab text for vertical strips \cr }
}
\examples{
p <- qplot(mpg, wt, data = mtcars)
p
p + opts(panel_background = theme_rect(colour = "pink"))
p + theme_bw()

# Scatter plot of gas milage by vehicle weight
p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
# Calculate slope and intercept of line of best fit
coef(lm(mpg ~ wt, data = mtcars))
p + geom_abline(intercept = 37, slope = -5)
# Calculate correlation coefficient
with(mtcars, cor(wt, mpg, use = "everything", method = "pearson"))
#annotate the plot
p + geom_abline(intercept = 37, slope = -5) +
geom_text(data = data.frame(), aes(4.5, 30, label = "Pearson-R = -.87"))

# Change the axis labels
# Original plot
p
p + xlab("Vehicle Weight") + ylab("Miles per Gallon")
# Or
p + labs(x = "Vehicle Weight", y = "Miles per Gallon")

# Add a title to the plot
p + opts(title = "Vehicle Weight-Gas Mileage Relationship")
# Change title appearance
p <- p + opts(title = "Vehicle Weight-Gas Mileage Relationship")
p + opts(plot.title = theme_text(size = 20))
p + opts(plot.title = theme_text(size = 20, colour = "Blue"))

# Changing plot look with themes
m <- ggplot(movies, aes(x = rating)) + geom_histogram()
#default is theme_grey()
m
# Compare with
m + theme_bw()

# Manipulate Axis Attributes
library(grid) # for unit
m + opts(axis.line = theme_segment())
m + opts(axis.line = theme_segment(colour = "red", linetype = "dotted"))
m + opts(axis.text.x = theme_text(colour = "blue"))
m + opts(axis.text.y = theme_blank())
m + opts(axis.ticks = theme_segment(size = 2))
m + opts(axis.title.y = theme_text(size = 20, angle = 90))
m + opts(axis.title.x = theme_blank())
m + opts(axis.ticks.length = unit(.85, "cm"))

# Legend Attributes
z <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point()
z
z + opts(legend.position = "none")
z + opts(legend.position = "bottom")
# Or use relative coordinates between 0 and 1
z + opts(legend.position = c(.5, .5))
z + opts(legend.background = theme_rect())
# Legend margin controls extra space around outside of legend:
z + opts(legend.background = theme_rect(), legend.margin = unit(1, "cm"))
z + opts(legend.background = theme_rect(), legend.margin = unit(0, "cm"))
# Or to just the keys
z + opts(legend.key = theme_rect())
z + opts(legend.key = theme_rect(fill = "yellow"))
z + opts(legend.key.size = unit(2.5, "cm"))
z + opts(legend.text = theme_text(size = 20, colour = "red", angle = 45))
z + opts(legend.title = theme_text(face = "italic"))

# To change the title of the legend use the name argument
# in one of the scale options
z + scale_colour_brewer(name = "My Legend")
z + scale_colour_grey(name = "Number of \\nCylinders")

# Panel and Plot Attributes
z + opts(panel.background = theme_rect())
z + opts(panel.background = theme_rect(fill = "black"))
z + opts(panel.border = theme_rect(linetype = "dashed"))
z + opts(panel.grid.major = theme_line(colour = "blue"))
z + opts(panel.grid.minor = theme_line(colour = "red", linetype = "dotted"))
z + opts(panel.grid.major = theme_line(size = 2))
z + opts(plot.background = theme_rect())
z + opts(plot.background = theme_rect(fill = "grey"))

# Faceting Attributes
k <- ggplot(diamonds, aes(carat, ..density..)) +  geom_histogram(binwidth = 0.2) +
facet_grid(. ~ cut)
k + opts(strip.background = theme_rect(colour = "purple", fill = "pink", size = 3, linetype = "dashed"))
k + opts(strip.text.x = theme_text(colour = "red", angle = 45, size = 10, hjust = 0.5, vjust = 0.5))
k + opts(panel.margin = unit(5, "lines"))
k + opts(panel.margin = unit(0, "lines"))
}

