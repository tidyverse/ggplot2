\name{ScaleSize}
\alias{scale_size}
\alias{ScaleSize}
\title{scale_size}
\description{Size scale for continuous variable}
\details{
This page describes \code{\link{scale_size}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_size(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \code{\link{scale_discrete}}: Discrete position scales
  \item \url{http://had.co.nz/ggplot/scale_size.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    (p <- qplot(mpg, cyl, data=mtcars, size=cyl))
    p + scale_size("cylinders")
    p + scale_size("number\nof\ncylinders")
    
    p + scale_size(to = c(0, 10))
    p + scale_size(to = c(1, 2))

    # Map area, instead of width/radius
    # Perceptually, this is a little better
    p + scale_area()
    p + scale_area(to = c(1, 25))
    
    # Also works with factors, but not a terribly good
    # idea, unless your factor is ordered, as in this example
    qplot(mpg, cyl, data=mtcars, size=factor(cyl))
    
    # For lines, you need to tell that you want lines on the legend
    (p <- qplot(mpg, cyl, data=mtcars, size=cyl, geom="line"))
    p + scale_size(guide="line")
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
