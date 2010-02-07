\name{scale_discrete}
\alias{scale_discrete}
\alias{scale_x_discrete}
\alias{scale_y_discrete}
\alias{scale_z_discrete}
\alias{ScaleDiscretePosition}
\title{scale\_discrete}
\description{Discrete position scale}
\details{
This page describes scale\_discrete, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_x_discrete(name = NULL, expand = c(0.05, 0.55), limits = NULL, 
    breaks = NULL, labels = NULL, formatter = identity, drop = FALSE, 
    legend = TRUE, ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis.  Maybe be an expression: see ?plotmath}
 \item{expand}{numeric vector of length 2, giving multiplicative and additive expansion factors}
 \item{limits}{numeric vector of length 2, giving the extent of the scale}
 \item{breaks}{numeric vector indicating where breaks should lie}
 \item{labels}{character vector giving labels associated with breaks}
 \item{formatter}{NULL}
 \item{drop}{NULL}
 \item{legend}{NULL}
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot2/scale_discrete.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
qplot(cut, data=diamonds, stat="bin")
qplot(cut, data=diamonds, geom="bar")

# The discrete position scale is added automatically whenever you
# have a discrete position.

(d <- qplot(cut, clarity, data=subset(diamonds, carat > 1), geom="jitter"))

d + scale_x_discrete("Cut")
d + scale_x_discrete("Cut", labels=c("F","G","VG","P","I"))

d + scale_y_discrete("Clarity")
d + scale_x_discrete("Cut") + scale_y_discrete("Clarity")

# Use limits to adjust the which levels (and in what order)
# are displayed
d + scale_x_discrete(limits=c("Fair","Ideal"))

# you can also use the short hand functions xlim and ylim
d + xlim("Fair","Ideal", "Good")
d + ylim("I1", "IF")

# See ?reorder to reorder based on the values of another variable
qplot(manufacturer, cty, data=mpg)
qplot(reorder(manufacturer, cty), cty, data=mpg)
qplot(reorder(manufacturer, displ), cty, data=mpg)

# Use abbreviate as a formatter to reduce long names
qplot(reorder(manufacturer, cty), cty, data=mpg) +  
  scale_x_discrete(formatter = "abbreviate")

}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
