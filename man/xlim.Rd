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
  Observations not in this range will be dropped completely
  and not passed to other layers.
}
\examples{
xlim(15, 20)
xlim(20, 15)
xlim(c(10, 20))
xlim("a", "b", "c")
qplot(mpg, wt, data=mtcars) + xlim(15, 20)
}
\seealso{
  For changing x or y axis limits \strong{without} dropping
  data observations, see \code{\link{coord_cartesian}}.
}

