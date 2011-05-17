\name{scale_colour_identity}
\alias{scale_colour_identity}
\title{Use values without scaling.}
\usage{scale_colour_identity(...)}

\description{
  Use values without scaling.
}
\examples{colour <- c("red", "green", "blue", "yellow")
qplot(1:4, 1:4, fill = colour, geom = "tile")
qplot(1:4, 1:4, fill = colour, geom = "tile") + scale_fill_identity()

# To get a legend, you also need to supply the labels to
# be used on the legend
qplot(1:4, 1:4, fill = colour, geom = "tile") +
scale_fill_identity("trt", labels = letters[1:4], breaks = colour)

# cyl scaled to appropriate size
qplot(mpg, wt, data = mtcars, size = cyl)

# cyl used as point size
qplot(mpg, wt, data = mtcars, size = cyl) + scale_size_identity()}
