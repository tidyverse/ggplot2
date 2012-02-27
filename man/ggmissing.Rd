\name{ggmissing}
\alias{ggmissing}
\title{Create a plot to illustrate patterns of missing values.}
\usage{
  ggmissing(data, avoid = "stack", order = TRUE,
    missing.only = TRUE)
}
\arguments{
  \item{data}{input data.frame}

  \item{avoid}{whether missings should be stacked or
  dodged, see \code{\link{geom_bar}} for more details}

  \item{order}{if \code{TRUE}, order variables by number of
  missings}

  \item{missing.only}{if \code{TRUE}, only display
  variables with some missing data}
}
\description{
  The missing values plot is a useful tool to get a rapid
  overview of the number and pattern of missing values in a
  dataset. Its strength is much more apparent when used
  with interactive graphics, as you can see in Mondrian
  (\url{http://rosuda.org/mondrian}) where this plot was
  copied from.
}
\examples{
mmissing <- movies
mmissing[sample(nrow(movies), 1000), sample(ncol(movies), 5)] <- NA
ggmissing(mmissing)
ggmissing(mmissing, order=FALSE, missing.only = FALSE)
ggmissing(mmissing, avoid="dodge") + scale_y_sqrt()
}
\seealso{
  \code{\link{ggstructure}}, \code{\link{ggorder}}
}

