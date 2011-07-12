\name{scale_colour_grey}
\alias{scale_colour_grey}
\alias{scale_fill_grey}
\title{Sequential grey colour scale.}
\usage{
  scale_colour_grey(..., start = 0.2, end = 0.8, na.value =
  "red")

  scale_fill_grey(..., start = 0.2, end = 0.8, na.value =
  "grey50")
}
\arguments{
  \item{na.value}{Colour to use for missing values}
}
\description{
  Sequential grey colour scale.
}
\details{
  Based on \code{\link{gray.colors}}
}
\examples{
p <- qplot(mpg, wt, data=mtcars, colour=factor(cyl))
p + scale_colour_grey()
p + scale_colour_grey(end = 0)

# You may want to turn off the pale grey background with this scale
p + scale_colour_grey() + theme_bw()

# Colour of missing values is controlled with na.value:
miss <- factor(sample(c(NA, 1:5), nrow(mtcars), rep = T))
qplot(mpg, wt, data = mtcars, colour = miss) + scale_colour_grey()
qplot(mpg, wt, data = mtcars, colour = miss) +
scale_colour_grey(na.value = "green")
}

