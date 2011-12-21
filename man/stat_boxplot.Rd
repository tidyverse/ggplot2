\name{stat_boxplot}
\alias{stat_boxplot}
\title{Calculate components of box and whisker plot.}
\usage{
  stat_boxplot(mapping = NULL, data = NULL,
    geom = "boxplot", position = "dodge", na.rm = FALSE,
    coef = 1.5, ...)
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

