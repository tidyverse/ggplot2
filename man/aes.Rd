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
  \code{\link{aes_string}}
}

