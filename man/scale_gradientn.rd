\name{scale_gradientn}
\alias{scale_gradientn}
\alias{scale_colour_gradientn}
\alias{scale_fill_gradientn}
\alias{ScaleGradientn}
\title{scale\_gradientn}
\description{Smooth gradient between n colours}
\details{
This page describes scale\_gradientn, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_colour_gradientn(name=NULL, colours, values=NULL, rescale=TRUE, space="rgb", breaks=NULL, labels=NULL, limits=NULL, trans="identity", alpha=1, ...)
scale_fill_gradientn(name=NULL, colours, values=NULL, rescale=TRUE, space="rgb", breaks=NULL, labels=NULL, limits=NULL, trans="identity", alpha=1, ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis}
 \item{colours}{NULL}
 \item{values}{NULL}
 \item{rescale}{NULL}
 \item{space}{colour space to interpolate through, rgb or Lab, see ?colorRamp for details}
 \item{breaks}{numeric vector indicating where breaks should lie}
 \item{labels}{character vector giving labels associated with breaks}
 \item{limits}{numeric vector of length 2, giving the extent of the scale}
 \item{trans}{a transformer to use}
 \item{alpha}{alpha value to use for colours}
 \item{...}{other arguments}
}
\seealso{\itemize{
  \item \code{\link{scale_gradient}}: continuous colour scale with midpoint
  \item colorRamp: for details of interpolation algorithm
  \item \url{http://had.co.nz/ggplot2/scale_gradientn.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{


}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
