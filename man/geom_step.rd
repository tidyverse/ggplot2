\name{GeomStep}
\alias{geom_step}
\alias{GeomStep}
\title{geom_step}
\description{Connect observations by stairs}
\details{
Equivalent to plot(type='s').This page describes \code{\link{geom_step}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\section{Aesthetics}{
The following aesthetics can be used with geom_step.  Aesthetics are mapped to variables in the data with the \code{\link{aes}} function: \code{geom_step(\code{\link{aes}}(x = var))}
\itemize{
  \item \code{x}: x position (\strong{required}) 
  \item \code{y}: y position (\strong{required}) 
  \item \code{colour}: border colour 
  \item \code{size}: size 
  \item \code{linetype}: line type 
}
}
\usage{geom_step(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \code{\link{geom_line}}: Functional (ordered) lines
  \item \code{\link{geom_polygon}}: Filled paths (polygons)
  \item \code{\link{geom_segment}}: Line segments
  \item \url{http://had.co.nz/ggplot/geom_step.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    # Simple quantiles/ECDF from examples(plot)
    x <- sort(rnorm(47))
    qplot(x, 1:47, geom="step")
    plot(x, type="s")
    
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
