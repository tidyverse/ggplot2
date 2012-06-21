\name{xlim}
\alias{xlim}
\title{Convenience function to set the limits of the x axis.}
\usage{
  xlim(...)
}
\arguments{
  \item{...}{if numeric, will create a continuous scale, if
  factor or character, will create a discrete scale.}
}
\description{
  Convenience function to set the limits of the x axis. Any
  observations not in this range will be excluded from the
  plot.
}
\examples{
xlim(15, 20)
xlim(20, 15)
xlim(c(10, 20))
xlim("a", "b", "c")
qplot(mpg, wt, data=mtcars) + xlim(15, 20)
}
\seealso{
  \code{\link{coord_cartesian}} for adjusting x and y
  limits without removing data (i.e. zooming in/out).
}

