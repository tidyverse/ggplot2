\name{coord_trans}
\alias{coord_trans}
\title{Transformed cartesian coordinate system.}

\description{
  Transformed cartesian coordinate system.
}
\arguments{
  \item{ytrans}{transformer for x axis}
  \item{xtrans}{transformer for y axis}
}
\examples{# See ?geom_boxplot for other examples

# Three ways of doing transformating in ggplot:
#  * by transforming the data
qplot(log10(carat), log10(price), data=diamonds)
#  * by transforming the scales
qplot(carat, price, data=diamonds, log="xy")
qplot(carat, price, data=diamonds) + scale_x_log10() + scale_y_log10()
#  * by transforming the coordinate system:
qplot(carat, price, data=diamonds) + coord_trans(x = "log10", y = "log10")

# The difference between transforming the scales and
# transforming the coordinate system is that scale
# transformation occurs BEFORE statistics, and coordinate
# transformation afterwards.  Coordinate transformation also 
# changes the shape of geoms:

d <- subset(diamonds, carat > 0.5)
qplot(carat, price, data = d, log="xy") + 
geom_smooth(method="lm")
qplot(carat, price, data = d) + 
geom_smooth(method="lm") +
coord_trans(x = "log10", y = "log10")

# Here I used a subset of diamonds so that the smoothed line didn't
# drop below zero, which obviously causes problems on the log-transformed
# scale

# With a combination of scale and coordinate transformation, it's
# possible to do back-transformations:
library(scales)
qplot(carat, price, data=diamonds, log="xy") + 
geom_smooth(method="lm") + 
coord_trans(x = exp_trans(10), y = exp_trans(10))
# cf.
qplot(carat, price, data=diamonds) + geom_smooth(method = "lm")}
