\name{geom_area}
\alias{geom_area}
\title{Area plot.}
\usage{geom_area(mapping, data, stat="identity", position="stack",
    na.rm=FALSE, ...)}

\description{
  Area plot.
}

\details{
  An area plot is the continuous analog of a stacked bar
  chart (see \code{\link{geom_bar}}), and can be used to
  show how composition of the whole varies over the range
  of x.  Choosing the order in which different components
  is stacked is very important, as it becomes increasing
  hard to see the individual pattern as you move up the
  stack.

  An area plot is a special case of
  \code{\link{geom_ribbon}}, where the minimum of the range
  is fixed to 0, and the position adjustment defaults to
  position_stacked.
}
\examples{# see geom_ribbon}
