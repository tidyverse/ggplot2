\name{geom_jitter}
\alias{geom_jitter}
\title{Points, jittered to reduce overplotting.}
\usage{
  geom_jitter(mapping = NULL, data = NULL,
    stat = "identity", position = "jitter", na.rm = FALSE,
    ...)
}
\arguments{
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

  \item{na.rm}{If \code{FALSE} (the default), removes
  missing values with a warning.  If \code{TRUE} silently
  removes missing values.}

  \item{...}{other arguments passed on to
  \code{\link{layer}}. This can include aesthetics whose
  values you want to set, not map. See \code{\link{layer}}
  for more details.}
}
\description{
  The jitter geom is a convenient default for geom_point
  with position = 'jitter'.  See
  \code{\link{position_jitter}} to see how to adjust amount
  of jittering.
}
\section{Aesthetics}{
  \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom",
  "jitter")}
}
\examples{
p <- ggplot(mpg, aes(displ, hwy))
p + geom_point()
p + geom_point(position = "jitter")

# Add aesthetic mappings
p + geom_jitter(aes(colour = cyl))

# Vary parameters
p + geom_jitter(position = position_jitter(width = .5))
p + geom_jitter(position = position_jitter(height = .5))

# Use qplot instead
qplot(displ, hwy, data = mpg, geom = "jitter")
qplot(class, hwy, data = mpg, geom = "jitter")
qplot(class, hwy, data = mpg, geom = c("boxplot", "jitter"))
qplot(class, hwy, data = mpg, geom = c("jitter", "boxplot"))
}
\seealso{
  \code{\link{geom_point}} for regular, unjittered points,
  \code{\link{geom_boxplot}} for another way of looking at
  the conditional distribution of a variable,
  \code{\link{position_jitter}} for examples of using
  jittering with other geoms
}

