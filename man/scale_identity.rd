\name{ScaleIdentity}
\alias{scale_identity}
\alias{ScaleIdentity}
\alias{scale_colour_identity}
\alias{scale_fill_identity}
\alias{scale_size_identity}
\alias{scale_shape_identity}
\alias{scale_linetype_identity}
\title{scale_identity}
\description{Don't remap values, use directly}
\details{
This page describes \code{\link{scale_identity}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_identity(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/scale_identity.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    colour <- c("red","green","blue","yellow")
    qplot(1:4, 1:4, fill=colour, geom="tile")
    qplot(1:4, 1:4, fill=colour, geom="tile") + scale_fill_identity()
    
    # To get a legend, you also need to supply the labels to
    # be used on the legend, and the grob to draw them:
    # grob_tile, grob_line, or grob_point
    qplot(1:4, 1:4, fill=colour, geom="tile") + scale_fill_identity(labels=letters[1:4], guide="tile", name="trt")
    
    # cyl scaled to appropriate size
    qplot(mpg, wt, data=mtcars, size = cyl)

    # cyl used as point size
    qplot(mpg, wt, data=mtcars, size = cyl) + scale_size_identity()
  
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
