\name{scale_colour_identity}
\alias{scale_alpha_identity}
\alias{scale_color_identity}
\alias{scale_colour_identity}
\alias{scale_fill_identity}
\alias{scale_linetype_identity}
\alias{scale_shape_identity}
\alias{scale_size_identity}
\title{Use values without scaling.}
\usage{
  scale_colour_identity(..., guide = "none")

  scale_fill_identity(..., guide = "none")

  scale_shape_identity(..., guide = "none")

  scale_linetype_identity(..., guide = "none")

  scale_alpha_identity(..., guide = "none")

  scale_size_identity(..., guide = "none")

  scale_color_identity(..., guide = "none")
}
\arguments{
  \item{...}{Other arguments passed on to
  \code{\link{discrete_scale}} or
  \code{\link{continuous_scale}}}

  \item{guide}{Guide to use for this scale - defaults to
  \code{"none"}.}
}
\description{
  Use values without scaling.
}
\examples{
colour <- c("red", "green", "blue", "yellow")
qplot(1:4, 1:4, fill = colour, geom = "tile")
qplot(1:4, 1:4, fill = colour, geom = "tile") + scale_fill_identity()

# To get a legend guide, you also need to supply labels, and specify guide = "legend"
qplot(1:4, 1:4, fill = colour, geom = "tile") +
  scale_fill_identity("trt", labels = letters[1:4], breaks = colour,
   guide = legend)

# cyl scaled to appropriate size
qplot(mpg, wt, data = mtcars, size = cyl)

# cyl used as point size
qplot(mpg, wt, data = mtcars, size = cyl) + scale_size_identity()
}

