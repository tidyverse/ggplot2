\name{PositionFill}
\alias{position_fill}
\alias{PositionFill}
\title{position_fill}
\description{Stack overlapping objects on top of one another, and standardise have equal height}
\details{
This page describes \code{\link{position_fill}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{position_fill(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/position_fill.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    # See ?geom_bar and ?geom_area for more examples
    ggplot(mtcars, aes(x=factor(cyl), fill=factor(vs))) + geom_bar(position="fill")
      
    ggplot(diamonds, aes(x=price)) + geom_bar(position="fill")
    ggplot(diamonds, aes(x=price, fill=cut)) + geom_bar(position="fill")
    ggplot(diamonds, aes(x=price, fill=clarity)) + geom_bar(position="fill")
    ggplot(diamonds, aes(x=price, fill=color)) + geom_bar(position="fill")
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
