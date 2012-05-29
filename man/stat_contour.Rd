\name{stat_contour}
\alias{stat_contour}
\title{Calculate contours of 3d data.}
\usage{
  stat_contour(mapping = NULL, data = NULL, geom = "path",
    position = "identity", na.rm = FALSE, ...)
}
\arguments{
  \item{na.rm}{If \code{FALSE} (the default), removes
  missing values with a warning.  If \code{TRUE} silently
  removes missing values.}

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
  A data frame with additional column: \item{level}{height
  of contour}
}
\description{
  Calculate contours of 3d data.
}
\section{Aesthetics}{
  \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat",
  "contour")}
}
\examples{
\donttest{
# Generate data
library(reshape2) # for melt
volcano3d <- melt(volcano)
names(volcano3d) <- c("x", "y", "z")

# Basic plot
v <- ggplot(volcano3d, aes(x, y, z = z))
v + stat_contour()

# Setting bins creates evenly spaced contours in the range of the data
v + stat_contour(bins = 2)
v + stat_contour(bins = 10)

# Setting binwidth does the same thing, parameterised by the distance
# between contours
v + stat_contour(binwidth = 2)
v + stat_contour(binwidth = 5)
v + stat_contour(binwidth = 10)
v + stat_contour(binwidth = 2, size = 0.5, colour = "grey50") +
  stat_contour(binwidth = 10, size = 1)

# Add aesthetic mappings
v + stat_contour(aes(size = ..level..))
v + stat_contour(aes(colour = ..level..))

# Change scale
v + stat_contour(aes(colour = ..level..), size = 2) +
  scale_colour_gradient(low = "brown", high = "white")

# Set aesthetics to fixed value
v + stat_contour(colour = "red")
v + stat_contour(size = 2, linetype = 4)

# Try different geoms
v + stat_contour(geom="polygon", aes(fill=..level..))
v + geom_tile(aes(fill = z)) + stat_contour()

# Use qplot instead
qplot(x, y, z = z, data = volcano3d, geom = "contour")
qplot(x, y, z = z, data = volcano3d, stat = "contour", geom = "path")
}
}

