\name{scale_size_continuous}
\alias{scale_size}
\alias{scale_size_continuous}
\alias{scale_size_discrete}
\title{Size scale.}
\usage{
  scale_size_continuous(..., range = c(1, 6))

  scale_size(..., range = c(1, 6))

  scale_size_discrete(..., range = c(1, 6))
}
\arguments{
  \item{range}{a numeric vector of length 2 that specifies
  the minimum and maximum size of the plotting symbol after
  transformation.}

  \item{...}{common continuous scale parameters:
  \code{name}, \code{breaks}, \code{labels},
  \code{na.value}, \code{limits} and \code{trans}.  See
  \code{\link{continuous_scale}} for more details}
}
\description{
  Size scale.
}
\examples{
(p <- qplot(mpg, cyl, data=mtcars, size=cyl))
p + scale_size("cylinders")
p + scale_size("number\\nof\\ncylinders")

p + scale_size(range = c(0, 10))
p + scale_size(range = c(1, 2))

# Map area, instead of width/radius
# Perceptually, this is a little better
p + scale_area()
p + scale_area(range = c(1, 25))

# Also works with factors, but not a terribly good
# idea, unless your factor is ordered, as in this example
qplot(mpg, cyl, data=mtcars, size=factor(cyl))

# To control the size mapping for discrete variable, use
# scale_size_manual:
last_plot() + scale_size_manual(values=c(2,4,6))
}

