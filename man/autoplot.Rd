\name{autoplot}
\alias{autoplot}
\title{Create a complete ggplot appropriate to a particular data type}
\usage{
  autoplot(x, ...)
}
\arguments{
  \item{x}{an object}

  \item{...}{other arguments passed to specific methods}
}
\value{
  a ggplot object
}
\description{
  \code{autoplot} uses ggplot2 to draw a particular plot
  for a particular class in a single command. This defines
  the S3 generic that other classes and packages can
  extend.
}
\seealso{
  \code{\link{ggplot}} and \code{\link{fortify}}
}

