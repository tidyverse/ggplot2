\name{ggplot.data.frame}
\alias{ggplot.data.frame}
\title{Create a new ggplot plot from a data frame}
\usage{
  \method{ggplot}{data.frame} (data, mapping = aes(), ...,
    environment = globalenv())
}
\arguments{
  \item{data}{default data frame for plot}

  \item{mapping}{The aesthetic mapping, usually constructed
  with \code{\link{aes}} or \code{\link{aes_string}}. Only
  needs to be set at the layer level if you are overriding
  the plot defaults.}

  \item{...}{ignored}

  \item{environment}{in which evaluation of aesthetics
  should occur}
}
\description{
  Create a new ggplot plot from a data frame
}
\seealso{
  \url{http://had.co.nz/ggplot2}
}

