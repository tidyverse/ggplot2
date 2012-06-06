\name{theme}
\alias{theme}
\title{Set options/theme elements for a single plot}
\usage{
  theme(...)
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
  axis.title.x \tab x axis label \cr axis.title.y \tab y
  axis label \cr axis.ticks \tab axis tick marks \cr
  axis.ticks.length \tab tick mark length \cr
  axis.ticks.margin \tab tick mark margin spacing \cr
  axis.text.x \tab horizontal tick labels \cr axis.text.y
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
  plot.title \tab plot title (text appearance) \cr
  plot.margin \tab plot margins \cr strip.background \tab
  background of facet labels \cr strip.text.x \tab text for
  horizontal strips \cr strip.text.y \tab text for vertical
  strips \cr title \tab A string containing the title of
  the plot \cr }
}
\examples{
\donttest{
p <- qplot(mpg, wt, data = mtcars)
p
p + theme(panel_background = element_rect(colour = "pink"))
p + theme_bw()

# Scatter plot of gas mileage by vehicle weight
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
p + theme(title = "Vehicle Weight-Gas Mileage Relationship")
# Change title appearance
p <- p + theme(title = "Vehicle Weight-Gas Mileage Relationship")
p + theme(plot.title = element_text(size = 20))
p + theme(plot.title = element_text(size = 20, colour = "Blue"))

# Changing plot look with themes
DF <- data.frame(x = rnorm(400))
m <- ggplot(DF, aes(x = x)) + geom_histogram()
#default is theme_grey()
m
# Compare with
m + theme_bw()

# Manipulate Axis Attributes
library(grid) # for unit
m + theme(axis.line = element_segment())
m + theme(axis.line = element_segment(colour = "red", linetype = "dotted"))
m + theme(axis.text.x = element_text(colour = "blue"))
m + theme(axis.text.y = element_blank())
m + theme(axis.ticks = element_segment(size = 2))
m + theme(axis.title.y = element_text(size = 20, angle = 90))
m + theme(axis.title.x = element_blank())
m + theme(axis.ticks.length = unit(.85, "cm"))

# Legend Attributes
z <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point()
z
z + theme(legend.position = "none")
z + theme(legend.position = "bottom")
# Or use relative coordinates between 0 and 1
z + theme(legend.position = c(.5, .5))
z + theme(legend.background = element_rect())
# Legend margin controls extra space around outside of legend:
z + theme(legend.background = element_rect(), legend.margin = unit(1, "cm"))
z + theme(legend.background = element_rect(), legend.margin = unit(0, "cm"))
# Or to just the keys
z + theme(legend.key = element_rect())
z + theme(legend.key = element_rect(fill = "yellow"))
z + theme(legend.key.size = unit(2.5, "cm"))
z + theme(legend.text = element_text(size = 20, colour = "red", angle = 45))
z + theme(legend.title = element_text(face = "italic"))

# To change the title of the legend use the name argument
# in one of the scale options
z + scale_colour_brewer(name = "My Legend")
z + scale_colour_grey(name = "Number of \\nCylinders")

# Panel and Plot Attributes
z + theme(panel.background = element_rect())
z + theme(panel.background = element_rect(fill = "black"))
z + theme(panel.border = element_rect(linetype = "dashed"))
z + theme(panel.grid.major = element_line(colour = "blue"))
z + theme(panel.grid.minor = element_line(colour = "red", linetype = "dotted"))
z + theme(panel.grid.major = element_line(size = 2))
z + theme(plot.background = element_rect())
z + theme(plot.background = element_rect(fill = "grey"))

# Faceting Attributes
set.seed(4940)
dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
k <- ggplot(dsmall, aes(carat, ..density..)) +  geom_histogram(binwidth = 0.2) +
facet_grid(. ~ cut)
k + theme(strip.background = element_rect(colour = "purple", fill = "pink", size = 3, linetype = "dashed"))
k + theme(strip.text.x = element_text(colour = "red", angle = 45, size = 10, hjust = 0.5, vjust = 0.5))
k + theme(panel.margin = unit(5, "lines"))
k + theme(panel.margin = unit(0, "lines"))
}
}

