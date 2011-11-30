\name{stat_aggrhex}
\alias{stat_aggrhex}
\title{Aggregate 2d plane into hexagons.}
\usage{
  stat_aggrhex(mapping = NULL, data = NULL, geom = "hex",
  position = "identity", bins = 30, na.rm = FALSE, fun =
  mean, ...)
}
\description{
  Aggregate 2d plane into hexagons.
}
\examples{
d <- ggplot(diamonds, aes(carat, price))
d + stat_aggrhex()

# Specifying function
d + stat_aggrhex(fun = function(x) sum(x^2))
d + stat_aggrhex(fun = var)
}
\seealso{
  \code{\link{stat_aggr2d}} for rectangular aggregation.
  \code{\link{stat_bin2d}} for the hexagon-ing options.
}

