\name{ScaleDiscrete}
\alias{scale_discrete}
\alias{ScaleDiscrete}
\alias{scale_x_discrete}
\alias{scale_y_discrete}
\alias{scale_z_discrete}
\title{scale_discrete}
\description{Discrete position scale}
\details{
This page describes \code{\link{scale_discrete}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_discrete(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/scale_discrete.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    # The discrete position scale is added automatically whenever you
    # have a discrete position and the only thing you can do with it
    # is change the labels
    
    (d <- qplot(cut, clarity, data=diamonds, geom="jitter"))
    
    d + scale_x_discrete("Cut")
    d + scale_x_discrete("Cut", labels=c("F","G","VG","P","I"))
    d + scale_y_discrete("Clarity")
    d + scale_x_discrete("Cut") + scale_y_discrete("Clarity")
    
    # To adjust the order you must modify the underlying factor
    # see ?reorder for one approach to this
    
  
    
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
