\name{scale_gradientn}
\alias{scale_gradientn}
\alias{scale_colour_gradientn}
\alias{scale_fill_gradientn}
\alias{ScaleGradientn}
\alias{scale_color_gradientn}
\title{scale\_gradientn}
\description{Smooth gradient between n colours}
\details{
This page describes scale\_gradientn, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_colour_gradientn(name=NULL, colours, values=NULL, rescale=TRUE, space="rgb", breaks=NULL, labels=NULL, limits=NULL, trans="identity", ...)
scale_fill_gradientn(name=NULL, colours, values=NULL, rescale=TRUE, space="rgb", breaks=NULL, labels=NULL, limits=NULL, trans="identity", ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis.  Maybe be an expression: see ?plotmath}
 \item{colours}{NULL}
 \item{values}{NULL}
 \item{rescale}{NULL}
 \item{space}{colour space to interpolate through, rgb or Lab, see ?colorRamp for details}
 \item{breaks}{numeric vector indicating where breaks should lie}
 \item{labels}{character vector giving labels associated with breaks}
 \item{limits}{numeric vector of length 2, giving the extent of the scale}
 \item{trans}{a transformer to use}
 \item{...}{other arguments}
}
\seealso{\itemize{
  \item \code{\link{scale_gradient}}: continuous colour scale with midpoint
  \item \code{\link{colorRamp}}: for details of interpolation algorithm
  \item \url{http://had.co.nz/ggplot2/scale_gradientn.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
# scale_colour_gradient make it easy to use existing colour palettes

dsub <- subset(diamonds, x > 5 & x < 6 & y > 5 & y < 6)
dsub$diff <- with(dsub, sqrt(abs(x-y))* sign(x-y))
(d <- qplot(x, y, data=dsub, colour=diff))

d + scale_colour_gradientn(colour = rainbow(7))
breaks <- c(-0.5, 0, 0.5)
d + scale_colour_gradientn(colour = rainbow(7), 
  breaks = breaks, labels = format(breaks))

d + scale_colour_gradientn(colour = topo.colors(10))
d + scale_colour_gradientn(colour = terrain.colors(10))

# You can force them to be symmetric by supplying a vector of 
# values, and turning rescaling off
max_val <- max(abs(dsub$diff))
values <- seq(-max_val, max_val, length = 11)

d + scale_colour_gradientn(colours = topo.colors(10), 
  values = values, rescale = FALSE)
d + scale_colour_gradientn(colours = terrain.colors(10), 
  values = values, rescale = FALSE)


}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
