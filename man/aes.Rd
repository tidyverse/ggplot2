\name{aes}
\alias{aes}
\title{Generate aesthetic mappings that describe how variables in the data are
mapped to visual properties (aesthetics) of geoms.}
\usage{
  aes(x, y, ...)
}
\arguments{
  \item{x}{x value}

  \item{y}{y value}

  \item{...}{List of name value pairs giving aesthetics to
  map.}
}
\description{
  \code{aes} creates a list of unevaluated expressions.
  This function also performs partial name matching,
  converts color to colour, and old style R names to ggplot
  names (eg. pch to shape, cex to size)
}
\examples{
aes(x = mpg, y = wt)
aes(x = mpg ^ 2, y = wt / cyl)
}
\seealso{
  \code{\link{aes_string}} for passing quoted variable
  names.  \code{\link{aesthetic_colour_fill}},
  \code{\link{aesthetic_alpha}},
  \code{\link{aesthetic_group_order}},
  \code{\link{aesthetic_linetype}},
  \code{\link{aesthetic_size}},
  \code{\link{aesthetic_shape}} and
  \code{\link{aesthetic_position}} for more specific
  examples with different aesthetics.
}

