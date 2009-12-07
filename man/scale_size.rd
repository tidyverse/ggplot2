\name{scale_size}
\alias{scale_size}
\alias{ScaleSize}
\alias{scale_area}
\title{scale\_size}
\description{Size scale for continuous variable}
\details{
This page describes scale\_size, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_size(name=NULL, limits=NULL, breaks=NULL, labels=NULL, trans=NULL, to=c(1, 6), ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis.  Maybe be an expression: see ?plotmath}
 \item{limits}{numeric vector of length 2, giving the extent of the scale}
 \item{breaks}{numeric vector indicating where breaks should lie}
 \item{labels}{character vector giving labels associated with breaks}
 \item{trans}{a transformer to use}
 \item{to}{a numeric vector of length 2 that specifies the minimum and maximum size of the plotting symbol after transformation.}
 \item{...}{other arguments}
}
\seealso{\itemize{
  \item \code{\link{scale_manual}}: for sizing discrete variables
  \item \url{http://had.co.nz/ggplot2/scale_size.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
(p <- qplot(mpg, cyl, data=mtcars, size=cyl))
p + scale_size("cylinders")
p + scale_size("number\nof\ncylinders")

p + scale_size(to = c(0, 10))
p + scale_size(to = c(1, 2))

# Map area, instead of width/radius
# Perceptually, this is a little better
p + scale_area()
p + scale_area(to = c(1, 25))

# Also works with factors, but not a terribly good
# idea, unless your factor is ordered, as in this example
qplot(mpg, cyl, data=mtcars, size=factor(cyl))

# To control the size mapping for discrete variable, use 
# scale_size_manual:
last_plot() + scale_size_manual(values=c(2,4,6))

}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
