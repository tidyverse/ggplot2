\name{scale_identity}
\alias{scale_identity}
\alias{scale_colour_identity}
\alias{scale_fill_identity}
\alias{scale_size_identity}
\alias{scale_shape_identity}
\alias{scale_linetype_identity}
\alias{ScaleIdentity}
\alias{scale_color_identity}
\title{scale\_identity}
\description{Use values without scaling}
\details{
This page describes scale\_identity, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_colour_identity(name = NULL, breaks = NULL, labels = NULL, 
    formatter = NULL, legend = TRUE, ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis.  Maybe be an expression: see ?plotmath}
 \item{breaks}{numeric vector indicating where breaks should lie}
 \item{labels}{character vector giving labels associated with breaks}
 \item{formatter}{NULL}
 \item{legend}{NULL}
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot2/scale_identity.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
colour <- c("red", "green", "blue", "yellow")
qplot(1:4, 1:4, fill = colour, geom = "tile")
qplot(1:4, 1:4, fill = colour, geom = "tile") + scale_fill_identity()

# To get a legend, you also need to supply the labels to
# be used on the legend
qplot(1:4, 1:4, fill = colour, geom = "tile") +
  scale_fill_identity("trt", labels = letters[1:4], breaks = colour)

# cyl scaled to appropriate size
qplot(mpg, wt, data = mtcars, size = cyl)

# cyl used as point size
qplot(mpg, wt, data = mtcars, size = cyl) + scale_size_identity()
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
