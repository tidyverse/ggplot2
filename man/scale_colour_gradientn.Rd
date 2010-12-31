\name{scale_colour_gradientn}
\alias{scale_colour_gradientn}
\title{Smooth colour gradient between n colours...}
\usage{scale_colour_gradientn(..., colours, values, space="Lab")}

\description{
  Smooth colour gradient between n colours
}
\examples{# scale_colour_gradient make it easy to use existing colour palettes

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
values = values, rescale = identity, oob = identity)
d + scale_colour_gradientn(colours = terrain.colors(10), 
values = values, rescale = identity, oob = identity)}
