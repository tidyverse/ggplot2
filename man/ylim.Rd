\name{ylim}
\alias{ylim}
\title{Convenience function to set the limits of the y axis.}
\usage{
  ylim(...)
}
\arguments{
  \item{...}{if numeric, will create a continuous scale, if
  factor or character, will create a discrete scale.}
}
\description{
  Convenience function to set the limits of the y axis. Any
  observations not in this range will be excluded from the
  plot.
}
\examples{
ylim(15, 20)
ylim(c(10, 20))
ylim("a", "b", "c")
qplot(mpg, wt, data=mtcars) + ylim(15, 20)
}
\seealso{
  \code{\link{coord_cartesian}} for adjusting x and y
  limits without removing data (i.e. zooming in/out).
}

