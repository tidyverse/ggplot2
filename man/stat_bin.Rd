\name{stat_bin}
\alias{stat_bin}
\title{Bin data.}
\usage{
  stat_bin(mapping = NULL, data = NULL, geom = "bar",
    position = "stack", width = 0.9, drop = FALSE,
    right = FALSE, binwidth = NULL, origin = NULL,
    breaks = NULL, ...)
}
\arguments{
  \item{binwidth}{Bin width to use. Defaults to 1/30 of the
  range of the data}

  \item{breaks}{Actual breaks to use.  Overrides bin width
  and origin}

  \item{origin}{Origin of first bin}

  \item{width}{Width of bars when used with categorical
  data}

  \item{right}{If \code{TRUE}, right-closed, left-open, if
  \code{FALSE},}

  \item{drop}{If TRUE, remove all bins with zero counts}

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
  New data frame with additional columns:
  \item{count}{number of points in bin}
  \item{density}{density of points in bin, scaled to
  integrate to 1} \item{ncount}{count, scaled to maximum of
  1} \item{ndensity}{density, scaled to maximum of 1}
}
\description{
  Missing values are currently silently dropped.
}
\examples{
simple <- data.frame(x = rep(1:10, each = 2))
base <- ggplot(simple, aes(x))
# By default, right = TRUE, and intervals are of the form (a, b]
base + stat_bin(binwidth = 1, drop = FALSE, right = TRUE, col = "black")
# If right = FALSE intervals are of the form [a, b)
base + stat_bin(binwidth = 1, drop = FALSE, right = FALSE, col = "black")

m <- ggplot(movies, aes(x=rating))
m + stat_bin()
m + stat_bin(binwidth=0.1)
m + stat_bin(breaks=seq(4,6, by=0.1))
# See geom_histogram for more histogram examples

# To create a unit area histogram, use aes(y = ..density..)
(linehist <- m + stat_bin(aes(y = ..density..), binwidth=0.1,
  geom="line", position="identity"))
linehist + stat_density(colour="blue", fill=NA)

# Also works with categorical variables
ggplot(movies, aes(x=mpaa)) + stat_bin()
qplot(mpaa, data=movies, stat="bin")
}

