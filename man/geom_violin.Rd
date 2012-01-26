\name{geom_violin}
\alias{geom_violin}
\title{Violin plot.}
\usage{
  geom_violin(mapping = NULL, data = NULL,
    stat = "ydensity", position = "dodge", trim = TRUE,
    scale = "equal", ...)
}
\arguments{
  \item{trim}{If \code{TRUE} (default), trim the tails of
  the violins to the range of the data. If \code{FALSE},
  don't trim the tails.}

  \item{scale}{if "equal" (default), all violins have the
  same area (to be precise, they would have the same area
  if tails are not trimmed). If "count", the areas are
  scaled proportionally to the number of observations.}

  \item{mapping}{The aesthetic mapping, usually constructed
  with \code{\link{aes}} or \code{\link{aes_string}}. Only
  needs to be set at the layer level if you are overriding
  the plot defaults.}

  \item{data}{A layer specific dataset - only needed if you
  want to override the plot defaults.}

  \item{stat}{The statistical transformation to use on the
  data for this layer.}

  \item{position}{The position adjustment to use for
  overlappling points on this layer}

  \item{...}{other arguments passed on to
  \code{\link{layer}}. This can include aesthetics whose
  values you want to set, not map. See \code{\link{layer}}
  for more details.}
}
\description{
  Violin plot.
}
\examples{
\donttest{
p <- ggplot(mtcars, aes(factor(cyl), mpg))

p + geom_violin()
qplot(factor(cyl), mpg, data = mtcars, geom = "violin")

p + geom_violin() + geom_jitter(height = 0)
p + geom_violin() + coord_flip()
qplot(factor(cyl), mpg, data = mtcars, geom = "violin") +
  coord_flip()

# Scale maximum width proportional to sample size:
p + geom_violin(scale = "count")

# Default is to trim violins to the range of the data. To disable:
p + geom_violin(trim = FALSE)

# Use a smaller bandwidth for closer density fit (default is 1).
p + geom_violin(adjust = .5)

# Add aesthetic mappings
# Note that violins are automatically dodged when any aesthetic is
# a factor
p + geom_violin(aes(fill = cyl))
p + geom_violin(aes(fill = factor(cyl)))
p + geom_violin(aes(fill = factor(vs)))
p + geom_violin(aes(fill = factor(am)))

# Set aesthetics to fixed value
p + geom_violin(fill = "grey80", colour = "#3366FF")
qplot(factor(cyl), mpg, data = mtcars, geom = "violin",
  colour = I("#3366FF"))

# Scales vs. coordinate transforms -------
# Scale transformations occur before the density statistics are computed.
# Coordinate transformations occur afterwards.  Observe the effect on the
# number of outliers.
library(plyr) # to access round_any
m <- ggplot(movies, aes(y = votes, x = rating,
   group = round_any(rating, 0.5)))
m + geom_violin()
m + geom_violin() + scale_y_log10()
m + geom_violin() + coord_trans(y = "log10")
m + geom_violin() + scale_y_log10() + coord_trans(y = "log10")

# Violin plots with continuous x:
# Use the group aesthetic to group observations in violins
qplot(year, budget, data = movies, geom = "violin")
qplot(year, budget, data = movies, geom = "violin",
  group = round_any(year, 10, floor))
}
}

