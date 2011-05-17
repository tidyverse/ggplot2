\name{scale_colour_grey}
\alias{scale_colour_grey}
\title{Sequential grey colour scale.}
\usage{scale_colour_grey(..., start=0.2, end=0.8)}

\description{
  Sequential grey colour scale.
}

\details{
  Based on \code{\link{gray.colors}}
}
\examples{p <- qplot(mpg, wt, data=mtcars, colour=factor(cyl)) 
p + scale_colour_grey()
p + scale_colour_grey(end = 0)

# You may want to turn off the pale grey background with this scale
p + scale_colour_grey() + theme_bw()}
