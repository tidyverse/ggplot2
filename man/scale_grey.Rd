\name{scale_colour_grey}
\alias{scale_color_grey}
\alias{scale_colour_grey}
\alias{scale_fill_grey}
\title{Sequential grey colour scale.}
\usage{
  scale_colour_grey(..., start = 0.2, end = 0.8,
    na.value = "red")

  scale_fill_grey(..., start = 0.2, end = 0.8,
    na.value = "grey50")

  scale_color_grey(..., start = 0.2, end = 0.8,
    na.value = "red")
}
\arguments{
  \item{start}{gray value at low end of palette}

  \item{end}{gray value at high end of palette}

  \item{...}{Other arguments passed on to
  \code{\link{continuous_scale}} to control name, limits,
  breaks, labels and so forth.}

  \item{na.value}{Colour to use for missing values}
}
\description{
  Based on \code{\link{gray.colors}}
}
\examples{
p <- qplot(mpg, wt, data=mtcars, colour=factor(cyl))
p + scale_colour_grey()
p + scale_colour_grey(end = 0)

# You may want to turn off the pale grey background with this scale
p + scale_colour_grey() + theme_bw()

# Colour of missing values is controlled with na.value:
miss <- factor(sample(c(NA, 1:5), nrow(mtcars), rep = TRUE))
qplot(mpg, wt, data = mtcars, colour = miss) + scale_colour_grey()
qplot(mpg, wt, data = mtcars, colour = miss) +
  scale_colour_grey(na.value = "green")
}
\seealso{
  Other colour scales: \code{\link{scale_color_brewer}},
  \code{\link{scale_color_continuous}},
  \code{\link{scale_color_discrete}},
  \code{\link{scale_color_gradient}},
  \code{\link{scale_color_gradient2}},
  \code{\link{scale_color_gradientn}},
  \code{\link{scale_color_hue}},
  \code{\link{scale_colour_brewer}},
  \code{\link{scale_colour_continuous}},
  \code{\link{scale_colour_discrete}},
  \code{\link{scale_colour_gradient}},
  \code{\link{scale_colour_gradient2}},
  \code{\link{scale_colour_gradientn}},
  \code{\link{scale_colour_hue}},
  \code{\link{scale_fill_brewer}},
  \code{\link{scale_fill_continuous}},
  \code{\link{scale_fill_discrete}},
  \code{\link{scale_fill_gradient}},
  \code{\link{scale_fill_gradient2}},
  \code{\link{scale_fill_gradientn}},
  \code{\link{scale_fill_hue}}
}

