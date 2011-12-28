\name{summary.ggplot}
\alias{summary.ggplot}
\title{Displays a useful description of a ggplot object}
\usage{
  summary.ggplot(object, ...)
}
\arguments{
  \item{object}{An object of class ggplot.}

  \item{...}{Extra arguments to be passed to the function.}
}
\description{
  Displays a useful description of a ggplot object
}
\examples{
summary(qplot(mpg, wt, data=mtcars))
}

