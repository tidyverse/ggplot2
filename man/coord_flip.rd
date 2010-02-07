\name{coord_flip}
\alias{coord_flip}
\alias{CoordFlip}
\title{coord\_flip}
\description{Flipped cartesian coordinates}
\details{
Flipped cartesian coordinates so that horizontal becomes vertical, and vertical, horizontal.  This is primarily useful for converting geoms and statistics which display y conditional on x, to x conditional on y

This page describes coord\_flip, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{coord_flip(xlim = NULL, ylim = NULL, ...)}
\arguments{
 \item{xlim}{NULL}
 \item{ylim}{NULL}
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot2/coord_flip.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
# Very useful for creating boxplots, and other interval
# geoms in the horizontal instead of vertical position.
qplot(cut, price, data=diamonds, geom="boxplot")
last_plot() + coord_flip()

qplot(cut, data=diamonds, geom="bar")
last_plot() + coord_flip()

qplot(carat, data=diamonds, geom="histogram")
last_plot() + coord_flip()

# You can also use it to flip lines and area plots:
qplot(1:5, (1:5)^2, geom="line")
last_plot() + coord_flip()
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
