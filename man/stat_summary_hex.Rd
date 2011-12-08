\name{stat_summary_hex}
\alias{stat_summary_hex}
\title{Apply functions to hexagonal bins.}
\usage{
  stat_summary_hex(mapping = NULL, data = NULL, geom =
  "hex", position = "identity", bins = 30, na.rm = FALSE,
  fun = mean, ...)
}
\description{
  Apply functions to hexagonal bins.
}
\examples{
d <- ggplot(diamonds, aes(carat, price))
d + stat_summary_hex()

# Specifying function
d + stat_summary_hex(fun = function(x) sum(x^2))
d + stat_summary_hex(fun = var, na.rm = T)
}
\seealso{
  \code{\link{stat_summary2d}} for rectangular
  summarization. \code{\link{stat_bin2d}} for the
  hexagon-ing options.
}

