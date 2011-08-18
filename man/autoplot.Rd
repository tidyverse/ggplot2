\name{autoplot}
\alias{autoplot}
\title{Create a complete ggplot appropriate to a particular data type}
\usage{
  \method{autoplot}{default} (x, ...)
}
\arguments{
  \item{x}{an object}

  \item{...}{other arguments passed to specific methods}
}
\value{
  a ggplot object
}
\description{
  autoplot uses ggplot2 to draw a particular plot for a
  particular class in a single command. This defines the S3
  generic that other classes and packages can extend.
}
\seealso{
  \url{http://had.co.nz/ggplot2}

  \code{\link{ggplot}}

  \code{\link{fortify}}
}

