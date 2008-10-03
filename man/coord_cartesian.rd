\name{coord_cartesian}
\alias{coord_cartesian}
\alias{CoordCartesian}
\title{coord\_cartesian}
\description{Cartesian coordinates}
\details{
The Cartesian coordinate system is the most familiar, and common, type of coordinate system.  There are no options to modify, and it is used by default, so you shouldn't need to call it explicitly

This page describes coord\_cartesian, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{coord_cartesian(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot2/coord_cartesian.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
# There aren't any parameters that you can control with 
# the Cartesian coordinate system, and they're the default, so
# you should never need to use it explicitly.  Most of the configuration
# of the axes and gridlines occurs in the scales, so look at 
# scale_continuous and scale_discrete for ideas.

qplot(rating, length, data=movies) + coord_cartesian()
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
