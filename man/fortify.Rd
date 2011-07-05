\name{fortify}
\alias{fortify}
\title{Fortify a model with data.}
\usage{
  fortify(model, data, ...)
}
\arguments{
  \item{model}{model or other R object to convert to data
  frame}

  \item{data}{original dataset, if needed}

  \item{...}{other arguments passed to methods}
}
\description{
  Fortify a model with data.
}
\details{
  Method to convert a generic R object into a data frame
  useful for plotting. Takes its name from the idea of
  fortifying the original data with model fit statistics,
  and vice versa.
}
\seealso{
  \code{\link{fortify.lm}}
}

