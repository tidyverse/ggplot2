\name{scale_pow}
\alias{scale_pow}
\alias{scale_x_pow}
\alias{scale_y_pow}
\alias{scale_z_pow}
\alias{scale_xend_pow}
\alias{scale_yend_pow}
\alias{ScalePow}
\title{scale\_pow}
\description{Power scale}
\details{
This page describes scale\_pow, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_x_pow(name=NULL, limits=c(NA, NA), breaks=NULL, power=1, ...)
scale_y_pow(name=NULL, limits=c(NA, NA), breaks=NULL, power=1, ...)
scale_z_pow(name=NULL, limits=c(NA, NA), breaks=NULL, power=1, ...)
scale_xend_pow(name=NULL, limits=c(NA, NA), breaks=NULL, power=1, ...)
scale_yend_pow(name=NULL, limits=c(NA, NA), breaks=NULL, power=1, ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis}
 \item{limits}{numeric vector of length 2, giving the extent of the scale}
 \item{breaks}{numeric vector indicating where breaks should lie}
 \item{power}{NULL}
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \code{\link{scale_discrete}}: Discrete position scales
  \item \url{http://had.co.nz/ggplot/scale_pow.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    # Coming soon
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
