\name{scale_grey}
\alias{scale_grey}
\alias{scale_colour_grey}
\alias{scale_fill_grey}
\alias{ScaleGrey}
\title{scale\_grey}
\description{Sequential grey colour scale}
\details{
Based on ?gray.colors

This page describes scale\_grey, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_colour_grey(name=NULL, start=0.2, end=0.8, limits=NULL, breaks=NULL, labels=NULL, formatter=identity, ...)
scale_fill_grey(name=NULL, start=0.2, end=0.8, limits=NULL, breaks=NULL, labels=NULL, formatter=identity, ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis.  Maybe be an expression: see ?plotmath}
 \item{start}{starting grey colour (between 0 and 1)}
 \item{end}{ending grey colour (between 0 and 1)}
 \item{limits}{numeric vector of length 2, giving the extent of the scale}
 \item{breaks}{numeric vector indicating where breaks should lie}
 \item{labels}{character vector giving labels associated with breaks}
 \item{formatter}{NULL}
 \item{...}{other arguments}
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot2/scale_grey.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
p <- qplot(mpg, wt, data=mtcars, colour=factor(cyl)) 
p + scale_colour_grey()
p + scale_colour_grey(end = 0)

# You may want to turn off the pale grey background with this scale
p + scale_colour_grey() + theme_bw
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
