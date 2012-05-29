\name{stat_density2d}
\alias{stat_density2d}
\title{2d density estimation.}
\usage{
  stat_density2d(mapping = NULL, data = NULL,
    geom = "density2d", position = "identity",
    na.rm = FALSE, contour = TRUE, n = 100, ...)
}
\arguments{
  \item{contour}{If \code{TRUE}, contour the results of the
  2d density estimation}

  \item{n}{number of grid points in each direction}

  \item{...}{other arguments passed on to
  \code{\link{kde2d}}}

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
}
\value{
  A data frame in the same format as
  \code{\link{stat_contour}}
}
\description{
  2d density estimation.
}
\section{Aesthetics}{
  \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat",
  "density2d")}
}
\examples{
\donttest{
library("MASS")
data(geyser, "MASS")

m <- ggplot(geyser, aes(x = duration, y = waiting)) +
  geom_point() + xlim(0.5, 6) + ylim(40, 110)
m + geom_density2d()

dens <- kde2d(geyser$duration, geyser$waiting, n = 50,
              lims = c(0.5, 6, 40, 110))
densdf <- data.frame(expand.grid(duration = dens$x, waiting = dens$y),
 z = as.vector(dens$z))
m + geom_contour(aes(z=z), data=densdf)

m + geom_density2d() + scale_y_log10()
m + geom_density2d() + coord_trans(y="log10")

m + stat_density2d(aes(fill = ..level..), geom="polygon")

qplot(duration, waiting, data=geyser, geom=c("point","density2d")) +
  xlim(0.5, 6) + ylim(40, 110)

# If you map an aesthetic to a categorical variable, you will get a
# set of contours for each value of that variable
set.seed(4393)
dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
qplot(x, y, data = dsmall, geom = "density2d", colour = cut)
qplot(x, y, data = dsmall, geom = "density2d", linetype = cut)
qplot(carat, price, data = dsmall, geom = "density2d", colour = cut)
d <- ggplot(dsmall, aes(carat, price)) + xlim(1,3)
d + geom_point() + geom_density2d()

# If we turn contouring off, we can use use geoms like tiles:
d + stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE)
last_plot() + scale_fill_gradient(limits=c(1e-5,8e-4))

# Or points:
d + stat_density2d(geom="point", aes(size = ..density..), contour = FALSE)
}
}

