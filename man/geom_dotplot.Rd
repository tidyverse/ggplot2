\name{geom_dotplot}
\alias{geom_dotplot}
\title{Dot plot}
\usage{
  geom_dotplot(mapping = NULL, data = NULL,
    stat = "bindot", position = "identity", na.rm = FALSE,
    binaxis = "x", method = "dotdensity",
    binpositions = "bygroup", stackdir = "up",
    stackratio = 1, dotsize = 1, ...)
}
\arguments{
  \item{binaxis}{which axis to bin along "x" (default) or
  "y"}

  \item{method}{"dotdensity" (default) for dot-density
  binning, or "histodot" for fixed bin widths (like
  stat_bin)}

  \item{binwidth}{When \code{method} is "dotdensity", this
  specifies maximum bin width.  When method is "histodot",
  this specifies bin width.  Defaults to 1/30 of the range
  of the data}

  \item{binpositions}{When \code{method} is "dotdensity",
  "bygroup" (default) determines positions of the bins for
  each group separately. "all" determines positions of the
  bins with all the data taken together; this is used for
  aligning dot stacks across multiple groups.}

  \item{stackdir}{which direction to stack the dots. "up"
  (default), "down", "center", "centerwhole" (centered, but
  with dots aligned)}

  \item{stackratio}{how close to stack the dots. Default is
  1, where dots just just touch. Use smaller values for
  closer, overlapping dots.}

  \item{dotsize}{The diameter of the dots relative to
  \code{binwidth}, default 1.}

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
  Dot plot
}
\examples{
ggplot(mtcars, aes(x = mpg)) + geom_dotplot()
ggplot(mtcars, aes(x = mpg)) + geom_dotplot(binwidth = 1.5)

# Use fixed-width bins
ggplot(mtcars, aes(x = mpg)) +
  geom_dotplot(method="histodot", binwidth = 1.5)

# Some other stacking methods
ggplot(mtcars, aes(x = mpg)) +
  geom_dotplot(binwidth = 1.5, stackdir = "center")
ggplot(mtcars, aes(x = mpg)) +
  geom_dotplot(binwidth = 1.5, stackdir = "centerwhole")

# y axis isn't really meaningful, so hide it
ggplot(mtcars, aes(x = mpg)) + geom_dotplot(binwidth = 1.5) +
  scale_y_continuous(name = "", breaks = NA)

# Overlap dots vertically
ggplot(mtcars, aes(x = mpg)) + geom_dotplot(binwidth = 1.5, stackratio = .7)

# Expand dot diameter
ggplot(mtcars, aes(x  =mpg)) + geom_dotplot(binwidth = 1.5, dotsize = 1.25)


# Examples with stacking along y axis instead of x
ggplot(mtcars, aes(x = 1, y = mpg)) +
  geom_dotplot(binaxis = "y", stackdir = "center")

ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_dotplot(binaxis = "y", stackdir = "center")

ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_dotplot(binaxis = "y", stackdir = "centerwhole")

ggplot(mtcars, aes(x = factor(vs), fill = factor(cyl), y = mpg)) +
  geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge")

# binpositions="all" ensures that the bins are aligned between groups
ggplot(mtcars, aes(x = factor(am), y = mpg)) +
  geom_dotplot(binaxis = "y", stackdir = "center", binpositions="all")
}

