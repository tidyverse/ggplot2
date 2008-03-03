\name{scale_shape}
\alias{scale_shape}
\alias{ScaleShape}
\title{scale\_shape}
\description{Discrete position scale}
\details{
This page describes scale\_shape, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_shape(name=NULL, solid=TRUE, ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis}
 \item{solid}{NULL}
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/scale_shape.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    dsmall <- diamonds[sample(nrow(diamonds), 100), ]
    
    (d <- qplot(carat, price, data=dsmall, shape=cut))
    d + scale_shape(solid = TRUE) # the default
    d + scale_shape(solid = FALSE)
    d + scale_shape(name="Cut of diamond")
    d + scale_shape(name="Cut of\ndiamond")
    
    # To change order of levels, change order of 
    # underlying factor
    levels(dsmall$cut) <- c("Fair", "Good", "Very Good", "Premium", "Ideal")

    # Need to recreate plot to pick up new data
    qplot(price, carat, data=dsmall, shape=cut)

    # Or for short:
    d %+% dsmall
    
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
