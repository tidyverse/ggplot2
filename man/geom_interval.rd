\name{GeomInterval}
\alias{geom_interval}
\alias{GeomInterval}
\title{geom_interval}
\description{Base for all interval (range) geoms}
\details{
This page describes \code{\link{geom_interval}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\section{Aesthetics}{
The following aesthetics can be used with geom_interval.  Aesthetics are mapped to variables in the data with the \code{\link{aes}} function: \code{geom_interval(\code{\link{aes}}(x = var))}
\itemize{
  \item \code{x}: x position (\strong{required}) 
  \item \code{min}: minimum of interval (\strong{required}) 
  \item \code{max}: maximum of interval (\strong{required}) 
}
}
\usage{geom_interval(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/geom_interval.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
  # Coming soon
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
