\name{ScaleGradient2}
\alias{scale_gradient2}
\alias{ScaleGradient2}
\alias{scale_colour_gradient2}
\alias{scale_fill_gradient2}
\title{scale_gradient2}
\description{Smooth colour gradient, with midpoint}
\details{
This page describes \code{\link{scale_gradient2}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_gradient2(name=NULL, low=muted("red"), mid="white", high=muted("blue"), midpoint=0, space="rgb", limits=c(NA, NA), trans="identity", alpha=1, ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis}
 \item{low}{colour at low end of scale}
 \item{mid}{colour at mid point of scale}
 \item{high}{colour at high end of scale}
 \item{midpoint}{position of mid point of scale, defaults to 0}
 \item{space}{colour space to interpolate through, rgb or Lab, see ?colorRamp for details}
 \item{limits}{numeric vector of length 2, giving the extent of the scale}
 \item{trans}{a transformer to use}
 \item{alpha}{alpha value to use for colours}
 \item{...}{other arguments}
}
\seealso{\itemize{
  \item \code{\link{scale_gradient}}: continuous colour scale with midpoint
  \item colorRamp: for details of interpolation algorithm
  \item \url{http://had.co.nz/ggplot/scale_gradient2.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    dsub <- subset(diamonds, x > 5 & x < 6 & y > 5 & y < 6)
    dsub$diff <- with(dsub, sqrt(abs(x-y))* sign(x-y))
    (d <- qplot(x, y, data=dsub, colour=diff))
    
    d + scale_colour_gradient2()
    # Change scale name
    d + scale_colour_gradient2(expression(sqrt(abs(x - y))))
    d + scale_colour_gradient2("Difference\nbetween\nwidth and\nheight")

    # Change limits and colours
    d + scale_colour_gradient2(limits=c(-0.2, 0.2))

    # Using "muted" colours makes for pleasant graphics 
    # (and they have better perceptual properties too)
    d + scale_colour_gradient2(low="red", high="blue")
    d + scale_colour_gradient2(low=muted("red"), high=muted("blue"))

    # Using the Lab colour space also improves perceptual properties
    # at the price of slightly slower operation
    d + scale_colour_gradient2(space="Lab")
    
    # About 5% of males are red-green colour blind, so it's a good
    # idea to avoid that combination
    d + scale_colour_gradient2(high=muted("green"))

    # We can also make the middle stand out
    d + scale_colour_gradient2(mid=muted("green"), high="white", low="white")
    
    # or use a non zero mid point
    (d <- qplot(carat, price, data=diamonds, colour=price/carat))
    d + scale_colour_gradient2(midpoint=mean(diamonds$price / diamonds$carat))
    
    # Fill gradients work much the same way
    p <- qplot(letters[1:5], 1:5, fill= c(-3, 3, 5, 2, -2), geom="bar")
    p + scale_fill_gradient2("fill")
    # Note how positive and negative values of the same magnitude
    # have similar intensity
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
