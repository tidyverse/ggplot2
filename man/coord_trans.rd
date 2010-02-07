\name{coord_trans}
\alias{coord_trans}
\alias{CoordTrans}
\title{coord\_trans}
\description{Transformed cartesian coordinate system}
\details{
This page describes coord\_trans, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{coord_trans(xtrans = "identity", ytrans = "identity", ...)}
\arguments{
 \item{xtrans}{NULL}
 \item{ytrans}{NULL}
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot2/coord_trans.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
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
qplot(carat, price, data=diamonds, log="xy") + 
  geom_smooth(method="lm") + 
  coord_trans(x="pow10", y="pow10")
# cf.
qplot(carat, price, data=diamonds) + geom_smooth(method = "lm")

}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
