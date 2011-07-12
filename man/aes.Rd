\name{aes}
\alias{aes}
\title{Generate aesthetic mappings that describe how variables in the data are...}
\usage{
  aes(x, y, ...)
}
\arguments{
  \item{x}{x value}

  \item{y}{y value}

  \item{List}{of name value pairs}
}
\description{
  Generate aesthetic mappings that describe how variables
  in the data are mapped to visual properties (aesthetics)
  of geoms.
}
\details{
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

