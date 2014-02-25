\name{benchplot}
\alias{benchplot}
\title{Benchmark plot creation time.
Broken down into construct, build, render and draw times.}
\usage{
benchplot(x)
}
\arguments{
  \item{x}{code to create ggplot2 plot}
}
\description{
Benchmark plot creation time. Broken down into construct,
build, render and draw times.
}
\examples{
benchplot(qplot(mpg, wt, data = mtcars))
benchplot(qplot(mpg, wt, data = mtcars) + facet_grid(.~ cyl))
}
\keyword{internal}

