\name{scale_colour_manual}
\alias{scale_alpha_manual}
\alias{scale_color_manual}
\alias{scale_colour_manual}
\alias{scale_fill_manual}
\alias{scale_linetype_manual}
\alias{scale_shape_manual}
\alias{scale_size_manual}
\title{Create your own discrete scale.}
\usage{
  scale_colour_manual(..., values)

  scale_fill_manual(..., values)

  scale_size_manual(..., values)

  scale_shape_manual(..., values)

  scale_linetype_manual(..., values)

  scale_alpha_manual(..., values)

  scale_color_manual(..., values)
}
\arguments{
  \item{values}{a set of aesthetic values to map data
  values to.}

  \item{...}{common discrete scale parameters: \code{name},
  \code{breaks}, \code{labels}, \code{na.value},
  \code{limits} and \code{guide}.  See
  \code{\link{discrete_scale}} for more details}
}
\description{
  Create your own discrete scale.
}
\examples{
\donttest{
p <- qplot(mpg, wt, data = mtcars, colour = factor(cyl))

p + scale_colour_manual(values = c("red","blue", "green"))
p + scale_colour_manual(
  values = c("8" = "red","4" = "blue","6" = "green"))
# With rgb hex values
p + scale_colour_manual(values = c("#FF0000", "#0000FF", "#00FF00"))

# As with other scales you can use breaks to control the appearance
# of the legend
cols <- c("8" = "red","4" = "blue","6" = "darkgreen", "10" = "orange")
p + scale_colour_manual(values = cols)
p + scale_colour_manual(values = cols, breaks = c("4", "6", "8"))
p + scale_colour_manual(values = cols, breaks = c("8", "6", "4"))
p + scale_colour_manual(values = cols, breaks = c("4", "6", "8"),
  labels = c("four", "six", "eight"))

# And limits to control the possible values of the scale
p + scale_colour_manual(values = cols, limits = c("4", "8"))
p + scale_colour_manual(values = cols, limits = c("4", "6", "8", "10"))
}
}

