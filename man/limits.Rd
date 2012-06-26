\name{xlim}
\alias{limits}
\alias{xlim}
\alias{ylim}
\title{Convenience functions to set the limits of the x and y axis.}
\usage{
  xlim(...)

  ylim(...)

  limits(lims, var)
}
\arguments{
  \item{...}{if numeric, will create a continuous scale, if
  factor or character, will create a discrete scale.}

  \item{limts}{vector of limits}

  \item{var}{name of variable}
}
\description{
  Observations not in this range will be dropped completely
  and not passed to any other layers.

  Generate correct scale type for specified limits
}
\examples{
xlim(15, 20)
xlim(20, 15)
xlim(c(10, 20))
xlim("a", "b", "c")
qplot(mpg, wt, data=mtcars) + xlim(15, 20)
ylim(15, 20)
ylim(c(10, 20))
ylim("a", "b", "c")
qplot(mpg, wt, data=mtcars) + ylim(15, 20)
ggplot2:::limits(c(1, 5), "x")
ggplot2:::limits(c(5, 1), "x")
ggplot2:::limits(c("A", "b", "c"), "x")
ggplot2:::limits(c("A", "b", "c"), "fill")
ggplot2:::limits(as.Date(c("2008-01-01", "2009-01-01")), "x")
}
\seealso{
  For changing x or y axis limits \strong{without} dropping
  data observations, see \code{\link{coord_cartesian}}.
}
\keyword{internal}

