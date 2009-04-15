\name{scale_hue}
\alias{scale_hue}
\alias{scale_colour_hue}
\alias{scale_fill_hue}
\alias{ScaleHue}
\alias{scale_colour_discrete}
\alias{scale_fill_discrete}
\alias{scale_color_hue}
\title{scale\_hue}
\description{Qualitative colour scale with evenly spaced hues}
\details{
This page describes scale\_hue, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_colour_hue(name=NULL, h=c(0, 360) + 15, l=65, c=100, limits=NULL, breaks=NULL, labels=NULL, h.start=0, direction=1, formatter=identity, ...)
scale_fill_hue(name=NULL, h=c(0, 360) + 15, l=65, c=100, limits=NULL, breaks=NULL, labels=NULL, h.start=0, direction=1, formatter=identity, ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis.  Maybe be an expression: see ?plotmath}
 \item{h}{range of hues to use, in [0, 360]}
 \item{l}{luminance (lightness), in [0, 100]}
 \item{c}{chroma (intensity of colour)}
 \item{limits}{numeric vector of length 2, giving the extent of the scale}
 \item{breaks}{numeric vector indicating where breaks should lie}
 \item{labels}{character vector giving labels associated with breaks}
 \item{h.start}{hue to start at}
 \item{direction}{direction to travel around the colour wheel, 1 = clockwise, -1 = counter-clockwise}
 \item{formatter}{NULL}
 \item{...}{other arguments}
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot2/scale_hue.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
(d <- qplot(carat, price, data=dsamp, colour=clarity))
  
# Change scale label
d + scale_colour_hue()
d + scale_colour_hue("clarity")
d + scale_colour_hue(expression(clarity[beta]))

# Adjust luminosity and chroma
d + scale_colour_hue(l=40, c=30)
d + scale_colour_hue(l=70, c=30)
d + scale_colour_hue(l=70, c=150)
d + scale_colour_hue(l=80, c=150)

# Change range of hues used
d + scale_colour_hue(h=c(0, 90))
d + scale_colour_hue(h=c(90, 180))
d + scale_colour_hue(h=c(180, 270))
d + scale_colour_hue(h=c(270, 360))

# Vary opacity
# (only works with pdf, quartz and cairo devices)
d <- ggplot(dsamp, aes(carat, price, colour = clarity))
d + geom_point(alpha = 0.9)
d + geom_point(alpha = 0.5)
d + geom_point(alpha = 0.2)
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
