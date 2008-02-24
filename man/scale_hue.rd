\name{ScaleHue}
\alias{scale_hue}
\alias{ScaleHue}
\alias{scale_colour_hue}
\alias{scale_fill_hue}
\title{scale_hue}
\description{Colours that vary continuously in hue}
\details{
This page describes \code{\link{scale_hue}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_hue(name=NULL, h=c(0, 360), l=65, c=100, alpha=1, labels=NULL, h.start=0, direction=1, ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis}
 \item{h}{range of hues to use, in degrees}
 \item{l}{luminance}
 \item{c}{chroma}
 \item{alpha}{alpha}
 \item{labels}{character vector giving labels associated with breaks}
 \item{h.start}{NULL}
 \item{direction}{NULL}
 \item{...}{other arguments}
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/scale_hue.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    (d <- qplot(carat, price, data=diamonds, colour=clarity))
  
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
    d + scale_colour_hue(alpha = 0.9)
    d + scale_colour_hue(alpha = 0.5)
    d + scale_colour_hue(alpha = 0.2)
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
