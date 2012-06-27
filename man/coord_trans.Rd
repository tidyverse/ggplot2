\name{coord_trans}
\alias{coord_trans}
\title{Transformed cartesian coordinate system.}
\usage{
  coord_trans(xtrans = "identity", ytrans = "identity",
    limx = NULL, limy = NULL)
}
\arguments{
  \item{xtrans,ytrans}{transformers for x and y axes}

  \item{limx,limy}{limits for x and y axes. (Named so for
  backward compatability)}
}
\description{
  \code{coord_trans} is different to scale transformations
  in that it occurs after statistical transformation and
  will affect the visual appearance of geoms - there is no
  guarantee that straight lines will continue to be
  straight.
}
\details{
  All current transformations only work with continuous
  values - see \code{\link[scales]{trans_new}} for list of
  transformations, and instructions on how to create your
  own.
}
\examples{
\donttest{
# See ?geom_boxplot for other examples

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
qplot(carat, price, data=diamonds) + geom_smooth(method = "lm")

# Also works with discrete scales
df <- data.frame(a = abs(rnorm(26)),letters)
plot <- ggplot(df,aes(a,letters)) + geom_point()

plot + coord_trans(x = "log10")
plot + coord_trans(x = "sqrt")
}
}

