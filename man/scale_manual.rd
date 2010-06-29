\name{scale_manual}
\alias{scale_manual}
\alias{scale_colour_manual}
\alias{scale_fill_manual}
\alias{scale_size_manual}
\alias{scale_shape_manual}
\alias{scale_linetype_manual}
\alias{ScaleManual}
\alias{scale_color_manual}
\title{scale\_manual}
\description{Create your own discrete scale}
\details{
This page describes scale\_manual, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_colour_manual(name = NULL, values = NULL, limits = NULL, 
    breaks = NULL, labels = NULL, formatter = identity, legend = TRUE, 
    ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis.  Maybe be an expression: see ?plotmath}
 \item{values}{NULL}
 \item{limits}{numeric vector of length 2, giving the extent of the scale}
 \item{breaks}{numeric vector indicating where breaks should lie}
 \item{labels}{character vector giving labels associated with breaks}
 \item{formatter}{NULL}
 \item{legend}{NULL}
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot2/scale_manual.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
p <- qplot(mpg, wt, data = mtcars, colour = factor(cyl))

p + scale_colour_manual(values = c("red","blue", "green"))
p + scale_colour_manual(
  values = c("8" = "red","4" = "blue","6" = "green"))

# As with other scales you can use breaks to control the appearance
# of the legend
cols <- c("8" = "red","4" = "blue","6" = "darkgreen", "10" = "orange")
p + scale_colour_manual(values = cols)
p + scale_colour_manual(values = cols, breaks = c("4", "6", "8"))
p + scale_colour_manual(values = cols, breaks = c("8", "6", "4"))
p + scale_colour_manual(values = cols, breaks = c("4", "6", "8"),
  labels = c("four", "six", "eight"))

# And limits to control the possible values of the scale
p + scale_colour_manual(values = cols, limits = c("4", "8"))
p + scale_colour_manual(values = cols, limits = c("4", "6", "8", "10"))

}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
