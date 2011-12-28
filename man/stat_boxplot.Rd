\name{stat_boxplot}
\alias{stat_boxplot}
\title{Calculate components of box and whisker plot.}
\usage{
  stat_boxplot(mapping = NULL, data = NULL,
    geom = "boxplot", position = "dodge", na.rm = FALSE,
    coef = 1.5, ...)
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

  \item{na.rm}{If \code{FALSE} (the default), removes
  missing values with a warning.  If \code{TRUE} silently
  removes missing values.}

  \item{coef}{Multiplier of the interquartile range to use
  when producing the whiskers. Default value: 1.5}

  \item{...}{other arguments passed on to the function.}
}
\value{
  A data frame with additional columns: \item{width}{width
  of boxplot} \item{ymin}{lower whisker = lower hinge - 1.5
  * IQR} \item{lower}{lower hinge, 25\% quantile}
  \item{notchlower}{lower edge of notch = median - 1.58 *
  IQR / sqrt(n)} \item{middle}{median, 50\% quantile}
  \item{notchupper}{upper edge of notch = median + 1.58 *
  IQR / sqrt(n)} \item{upper}{upper hinge, 75\% quantile}
  \item{ymax}{upper whisker = upper hinge + 1.5 * IQR}
}
\description{
  Calculate components of box and whisker plot.
}
\examples{
# See geom_boxplot for examples
}

