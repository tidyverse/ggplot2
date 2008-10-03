\name{scale_discrete}
\alias{scale_discrete}
\alias{scale_x_discrete}
\alias{scale_y_discrete}
\alias{scale_z_discrete}
\alias{ScaleDiscretePosition}
\title{scale\_discrete}
\description{Discrete position scale}
\details{
This page describes scale\_discrete, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_x_discrete(name=NULL, expand=c(0.05, 0), limits=NULL, breaks=NULL, labels=NULL, ...)
scale_y_discrete(name=NULL, expand=c(0.05, 0), limits=NULL, breaks=NULL, labels=NULL, ...)
scale_z_discrete(name=NULL, expand=c(0.05, 0), limits=NULL, breaks=NULL, labels=NULL, ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis}
 \item{expand}{numeric vector of length 2, giving multiplicative and additive expansion factors}
 \item{limits}{numeric vector of length 2, giving the extent of the scale}
 \item{breaks}{numeric vector indicating where breaks should lie}
 \item{labels}{character vector giving labels associated with breaks}
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot2/scale_discrete.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
# The discrete position scale is added automatically whenever you
# have a discrete position and the only thing you can do with it
# is change the labels

(d <- qplot(cut, clarity, data=subset(diamonds, carat > 1), geom="jitter"))

d + scale_x_discrete("Cut")
d + scale_x_discrete("Cut", labels=c("F","G","VG","P","I"))

d + scale_y_discrete("Clarity")
d + scale_x_discrete("Cut") + scale_y_discrete("Clarity")

# Use limits to adjust the which levels (and in what order)
# are displayed
d + scale_x_discrete(limits=c("Fair","Ideal"))
# See ?reorder to reorder based on the values of another variable

}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
