\name{stat_function}
\alias{stat_function}
\title{Superimpose a function.}
\usage{
  stat_function(mapping = NULL, data = NULL, geom = "path",
    position = "identity", fun, n = 101, args = list(),
    ...)
}
\arguments{
  \item{mapping}{The aesthetic mapping, usually constructed
  with \code{\link{aes}} or \code{\link{aes_string}}. Only
  needs to be set at the layer level if you are overriding
  the plot defaults.}

  \item{data}{A layer specific dataset - only needed if you
  want to override the plot defaults.}

  \item{geom}{The geom to apply to the data for this
  layer.}

  \item{position}{The position adjustment to use for
  overlapping points on this layer.}

  \item{fun}{function to use}

  \item{n}{number of points to interpolate along}

  \item{args}{list of additional arguments to pass to
  \code{fun}}

  \item{...}{other arguments passed on to the function.}
}
\value{
  a data.frame with additional columns: \item{x}{x's along
  a grid} \item{y}{value of function evaluated at
  corresponding x}
}
\description{
  Superimpose a function.
}
\examples{
x <- rnorm(100)
base <- qplot(x, geom="density")
base + stat_function(fun = dnorm, colour = "red")
base + stat_function(fun = dnorm, colour = "red", arg = list(mean = 3))
}

