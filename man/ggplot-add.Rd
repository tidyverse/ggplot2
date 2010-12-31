\name{ggplot-add}
\title{Modify a plot by adding on new components.}
\usage{\method{+}{ggplot} (p, object)}

\description{
  Modify a plot by adding on new components.
}

\details{
  What happens when you add on:

  \itemize{ \item \code{data.frame}: replace current
  data.frame (must use \code{\%+\%}) \item \code{uneval}:
  replace current aesthetics \item \code{layer}: add new
  layer \item \code{options}: update plot options \item
  \code{scale}: replace current scale \item \code{coord}:
  override current coordinate system \item \code{facet}:
  override current coordinate faceting }
}
\seealso{\code{\link{set_last_plot}}, \code{\link{ggplot}}}
\alias{+.ggplot}
\alias{%+%}
\arguments{
  \item{p}{plot object}
  \item{object}{component to add}
}
