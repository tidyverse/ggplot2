\name{+.ggplot}
\alias{\%+\%}
\alias{+.ggplot}
\title{Modify a plot by adding on new components.}
\usage{
  \method{+}{ggplot} (p, object)

  %+%(p, object)
}
\arguments{
  \item{p}{plot object}

  \item{object}{component to add}
}
\description{
  What happens when you add on:
}
\details{
  \itemize{ \item \code{data.frame}: replace current
  data.frame (must use \code{\%+\%}) \item \code{uneval}:
  replace current aesthetics \item \code{layer}: add new
  layer \item \code{options}: update plot options \item
  \code{scale}: replace current scale \item \code{coord}:
  override current coordinate system \item \code{facet}:
  override current coordinate faceting }
}
\seealso{
  \code{\link{ggplot}}
}

