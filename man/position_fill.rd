\name{position_fill}
\alias{position_fill}
\alias{PositionFill}
\title{position\_fill}
\description{Stack overlapping objects on top of one another, and standardise have equal height}
\details{
This page describes position\_fill, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{position_fill(width=NULL, height=NULL, ...)}
\arguments{
 \item{width}{NULL}
 \item{height}{NULL}
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot2/position_fill.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
# See ?geom_bar and ?geom_area for more examples
ggplot(mtcars, aes(x=factor(cyl), fill=factor(vs))) + geom_bar(position="fill")
  
cde <- geom_histogram(position="fill", binwidth = 500)
  
ggplot(diamonds, aes(x=price)) + cde
ggplot(diamonds, aes(x=price, fill=cut)) + cde
ggplot(diamonds, aes(x=price, fill=clarity)) + cde
ggplot(diamonds, aes(x=price, fill=color)) + cde
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
