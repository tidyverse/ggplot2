\name{GeomContour}
\alias{geom_contour}
\alias{GeomContour}
\title{geom_contour}
\description{Display contours of a 3d surface in 2d}
\details{
This page describes \code{\link{geom_contour}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\section{Aesthetics}{
The following aesthetics can be used with geom_contour.  Aesthetics are mapped to variables in the data with the \code{\link{aes}} function: \code{geom_contour(\code{\link{aes}}(x = var))}
\itemize{
  \item \code{x}: x position (\strong{required}) 
  \item \code{y}: y position (\strong{required}) 
  \item \code{weight}: observation weight used in statistical transformation 
  \item \code{colour}: border colour 
  \item \code{size}: size 
  \item \code{linetype}: line type 
}
}
\usage{geom_contour(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item geom_density2d: Draw 2d density contours
  \item \url{http://had.co.nz/ggplot/geom_contour.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    # See stat_contour for examples
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
