\name{StatFunction}
\alias{stat_function}
\alias{StatFunction}
\title{stat_function}
\description{Superimpose a function }
\details{
This page describes \code{\link{stat_function}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{stat_function(...)}
\arguments{
 \item{...}{other arguments}
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/stat_function.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    x <- rnorm(100)
    qplot(x, geom="density") + stat_function(fun = dnorm, colour="red")
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
