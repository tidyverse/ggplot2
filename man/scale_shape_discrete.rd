\name{scale_shape_discrete}
\alias{scale_shape_discrete}
\alias{ScaleShapeDiscrete}
\alias{scale_shape}
\title{scale\_shape\_discrete}
\description{Scale for shapes, aka glyphs}
\details{
This page describes scale\_shape\_discrete, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_shape_discrete(name = NULL, solid = TRUE, limits = NULL, 
    breaks = NULL, labels = NULL, formatter = identity, legend = TRUE, 
    ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis.  Maybe be an expression: see ?plotmath}
 \item{solid}{NULL}
 \item{limits}{numeric vector of length 2, giving the extent of the scale}
 \item{breaks}{numeric vector indicating where breaks should lie}
 \item{labels}{character vector giving labels associated with breaks}
 \item{formatter}{NULL}
 \item{legend}{NULL}
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot2/scale_shape_discrete.html}
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
