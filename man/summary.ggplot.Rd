\name{summary.ggplot}
\alias{summary.ggplot}
\title{Displays a useful description of a ggplot object}
\usage{
  summary.ggplot(object, ...)
}
\arguments{
  \item{object}{ggplot2 object to summarise}

  \item{...}{other arguments ignored (for compatibility
  with generic)}
}
\description{
  Displays a useful description of a ggplot object
}
\examples{
summary(qplot(mpg, wt, data=mtcars))
}
\keyword{internal}

