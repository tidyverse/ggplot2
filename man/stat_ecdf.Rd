\name{stat_ecdf}
\alias{stat_ecdf}
\title{Empirical Cumulative Density Function}
\usage{
  stat_ecdf(mapping = NULL, data = NULL, geom = "step",
    position = "identity", n = 101, ...)
}
\arguments{
  \item{n}{number of points to interpolate along}

  \item{mapping}{The aesthetic mapping, usually constructed
  with \code{\link{aes}} or \code{\link{aes_string}}. Only
  needs to be set at the layer level if you are overriding
  the plot defaults.}

  \item{data}{A layer specific dataset - only needed if you
  want to override the plot defaults.}

  \item{geom}{The geometric object to use display the data}

  \item{position}{The position adjustment to use for
  overlappling points on this layer}

  \item{...}{other arguments passed on to
  \code{\link{layer}}. This can include aesthetics whose
  values you want to set, not map. See \code{\link{layer}}
  for more details.}
}
\value{
  a data.frame with additional columns: \item{x}{x's along
  a grid} \item{y}{cumulative density corresponding x}
}
\description{
  Empirical Cumulative Density Function
}
\examples{
\donttest{
l(ggplot2, T)
set.seed(42)
x <- c(rnorm(1000))
qplot(x, stat = "ecdf", geom = "step")

df <- data.frame(x = c(rnorm(100, 0, 3), rnorm(100, 0, 10)),
                 g = gl(2, 100))

ggplot(df, aes(x, colour = g)) + stat_ecdf()
}
}

