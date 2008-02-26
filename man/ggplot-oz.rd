\name{"+.ggplot"}
\alias{+.ggplot}
\alias{\%+\%}
\title{Plot construction}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
The elements of a ggplot plot are combined together with addition.
}
\usage{"+.ggplot"(p, object)}
\arguments{
\item{p}{plot object}
\item{object}{}
}

\details{\itemize{
\item \code{data.frame}: replace default data.frame (must use \code{\%+\%})
\item \code{uneval}: replace default aesthetics
\item \code{layer}: add new layer
\item \code{options}: update plot options
\item \code{scale}: replace default scale
\item \code{coord}: override default coordinate system
\item \code{facet}: override default coordinate facetting
}}
\seealso{\code{\link{set_last_plot}}, \code{\link{ggplot}}}
\examples{}
\keyword{internal}
