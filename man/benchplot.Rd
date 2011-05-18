\name{benchplot}
\alias{benchplot}
\title{Benchmark plot creation time.}
\usage{benchplot(x)}

\description{
  Benchmark plot creation time. Broken down into construct,
  build, render and draw times.
}
\keyword{internal}
\arguments{
  \item{x}{code to create ggplot2 plot}
}
\examples{benchplot(qplot(mpg, wt, data = mtcars))
benchplot(qplot(mpg, wt, data = mtcars) + facet_grid(.~ cyl))}
