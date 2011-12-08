\name{stat_summary2d}
\alias{stat_summary2d}
\title{Apply function for rectangular bins.}
\usage{
  stat_summary2d(mapping = NULL, data = NULL, geom =
  "rect", position = "identity", bins = 30, drop = TRUE,
  fun = mean, ...)
}
\description{
  Apply function for rectangular bins.
}
\examples{
d <- ggplot(diamonds, aes(carat, depth, z = price))
d + stat_summary2d()

# Specifying function
d + stat_summary2d(fun = function(x) sum(x^2))
d + stat_summary2d(fun = var)
}
\seealso{
  \code{\link{stat_summary_hex}} for hexagonal
  summarization. \code{\link{stat_bin2d}} for the binning
  options.
}

