\name{ScaleGrey}
\alias{scale_grey}
\alias{ScaleGrey}
\alias{scale_colour_grey}
\alias{scale_fill_grey}
\title{scale_grey}
\description{Grey colour scale}
\details{
Based on ?gray.colors

This page describes \code{\link{scale_grey}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_grey(name=NULL, start=0.2, end=0.8, labels=NULL, ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis}
 \item{start}{starting grey colour (between 0 and 1)}
 \item{end}{ending grey colour (between 0 and 1)}
 \item{labels}{character vector giving labels associated with breaks}
 \item{...}{other arguments}
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/scale_grey.html}
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
