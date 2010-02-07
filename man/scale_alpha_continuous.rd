\name{scale_alpha_continuous}
\alias{scale_alpha_continuous}
\alias{ScaleAlphaContinuous}
\alias{scale_alpha}
\title{scale\_alpha\_continuous}
\description{Alpha scale for continuous variable}
\details{
This page describes scale\_alpha\_continuous, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_alpha_continuous(name = NULL, limits = NULL, breaks = NULL, 
    labels = NULL, trans = NULL, to = c(0.1, 1), legend = TRUE, 
    ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis.  Maybe be an expression: see ?plotmath}
 \item{limits}{numeric vector of length 2, giving the extent of the scale}
 \item{breaks}{numeric vector indicating where breaks should lie}
 \item{labels}{character vector giving labels associated with breaks}
 \item{trans}{a transformer to use}
 \item{to}{numeric vector of length 2, giving minimum and maximum after transformation}
 \item{legend}{NULL}
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \code{\link{scale_discrete}}: Discrete position scales
  \item \url{http://had.co.nz/ggplot2/scale_alpha_continuous.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
(p <- qplot(mpg, cyl, data=mtcars, alpha=cyl))
p + scale_alpha("cylinders")
p + scale_alpha("number\nof\ncylinders")

p + scale_alpha(to = c(0.4, 0.8))
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
