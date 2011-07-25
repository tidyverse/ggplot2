\name{scale_colour_gradient2}
\alias{scale_colour_gradient2}
\alias{scale_colour_gradientn}
\alias{scale_fill_gradient2}
\alias{scale_fill_gradientn}
\title{Diverging colour gradient}
\usage{
  scale_colour_gradient2(..., low = muted("red"), mid =
  "white", high = muted("blue"), midpoint = 0, space =
  "rgb", na.value = "grey50")

  scale_fill_gradient2(..., low = muted("red"), mid =
  "white", high = muted("blue"), midpoint = 0, space =
  "rgb", na.value = "grey50")

  scale_colour_gradientn(..., colours, values = NULL, space
  = "Lab", na.value = "grey50")

  scale_fill_gradientn(..., colours, values = NULL, space =
  "Lab", na.value = "grey50")
}
\arguments{
  \item{...}{Other arguments passed on to
  \code{\link{continuous_scale}} to control name, limits,
  breaks, labels and so forth.}

  \item{na.value}{Colour to use for missing values}

  \item{low}{colour for low end of gradient.}

  \item{mid}{colour for mid point}

  \item{high}{colour for high end of gradient.}

  \item{space}{colour space in which to calculate gradient.
  "Lab" usually best unless gradient goes through white.}

  \item{colours}{vector of colours}

  \item{values}{if colours should not be evenly positioned
  along the gradient}
}
\description{
  Diverging colour gradient

  Smooth colour gradient between n colours
}
\examples{
dsub <- subset(diamonds, x > 5 & x < 6 & y > 5 & y < 6)
dsub$diff <- with(dsub, sqrt(abs(x-y))* sign(x-y))
(d <- qplot(x, y, data=dsub, colour=diff))

d + scale_colour_gradient2()
# Change scale name
d + scale_colour_gradient2(expression(sqrt(abs(x - y))))
d + scale_colour_gradient2("Difference\\nbetween\\nwidth and\\nheight")

# Change limits and colours
d + scale_colour_gradient2(limits=c(-0.2, 0.2))

# Using "muted" colours makes for pleasant graphics
# (and they have better perceptual properties too)
d + scale_colour_gradient2(low="red", high="blue")
d + scale_colour_gradient2(low=muted("red"), high=muted("blue"))

# Using the Lab colour space also improves perceptual properties
# at the price of slightly slower operation
d + scale_colour_gradient2(space="Lab")

# About 5\% of males are red-green colour blind, so it's a good
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

# scale_colour_gradient make it easy to use existing colour palettes

dsub <- subset(diamonds, x > 5 & x < 6 & y > 5 & y < 6)
dsub$diff <- with(dsub, sqrt(abs(x-y))* sign(x-y))
(d <- qplot(x, y, data=dsub, colour=diff))

d + scale_colour_gradientn(colours = rainbow(7))
breaks <- c(-0.5, 0, 0.5)
d + scale_colour_gradientn(colours = rainbow(7),
breaks = breaks, labels = format(breaks))

d + scale_colour_gradientn(colours = topo.colors(10))
d + scale_colour_gradientn(colours = terrain.colors(10))

# You can force them to be symmetric by supplying a vector of
# values, and turning rescaling off
max_val <- max(abs(dsub$diff))
values <- seq(-max_val, max_val, length = 11)

d + scale_colour_gradientn(colours = topo.colors(10),
values = values, rescale = function(x, ...) x, oob = identity)
d + scale_colour_gradientn(colours = terrain.colors(10),
values = values, rescale = function(x, ...) x, oob = identity)
}
\seealso{
  Other colour scales: \code{\link{scale_colour_brewer}},
  \code{\link{scale_colour_gradient}},
  \code{\link{scale_colour_grey}},
  \code{\link{scale_colour_hue}},
  \code{\link{scale_fill_brewer}},
  \code{\link{scale_fill_gradient}},
  \code{\link{scale_fill_grey}},
  \code{\link{scale_fill_hue}}

  Other colour scales: \code{\link{scale_colour_brewer}},
  \code{\link{scale_colour_gradient}},
  \code{\link{scale_colour_grey}},
  \code{\link{scale_colour_hue}},
  \code{\link{scale_fill_brewer}},
  \code{\link{scale_fill_gradient}},
  \code{\link{scale_fill_grey}},
  \code{\link{scale_fill_hue}}
}

