\name{stat_ydensity}
\alias{stat_ydensity}
\title{1d kernel density estimate along y axis, for violin plot.}
\usage{
  stat_ydensity(mapping = NULL, data = NULL,
    geom = "violin", position = "dodge", adjust = 1,
    kernel = "gaussian", trim = TRUE, scale = "equal",
    na.rm = FALSE, ...)
}
\arguments{
  \item{trim}{If \code{TRUE} (default), trim the tails of
  the violins to the range of the data. If \code{FALSE},
  don't trim the tails.}

  \item{scale}{if "equal" (default), all violins have the
  same area (to be precise, they would have the same area
  if tails are not trimmed). If "count", the areas are
  scaled proportionally to the number of observations.}

  \item{adjust}{see \code{\link{density}} for details}

  \item{kernel}{kernel used for density estimation, see
  \code{\link{density}} for details}
}
\value{
  A data frame with additional columns: \item{width}{width
  of violin bounding box} \item{ydensity}{density estimate}
  \item{scaled}{density estimate, scaled depending on
  scalearea and scalecount} \item{count}{ydensity * number
  of points} \item{counttotal}{number of points}
}
\description{
  1d kernel density estimate along y axis, for violin plot.
}
\examples{
# See geom_violin for examples
# Also see stat_density for similar examples with data along x axis
}

