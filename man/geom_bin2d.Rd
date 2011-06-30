\name{geom_bin2d}
\alias{geom_bin2d}
\title{Add heatmap of 2d bin counts.}
\usage{geom_bin2d(mapping = NULL, data = NULL, stat = "bin2d",
    position = "identity", ...)}

\description{
  Add heatmap of 2d bin counts.
}
\examples{d <- ggplot(diamonds, aes(x = x, y = y)) + xlim(4,10) + ylim(4,10)
d + geom_bin2d()
d + geom_bin2d(binwidth = c(0.1, 0.1))

# See ?stat_bin2d for more examples}

