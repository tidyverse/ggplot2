\name{StatUnique}
\alias{stat_unique}
\alias{StatUnique}
\title{stat_unique}
\description{Remove duplicates}
\details{
This page describes \code{\link{stat_unique}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{stat_unique(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/stat_unique.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    ggplot(mtcars, aes(x=vs, y=am)) + geom_point(colour="#00000010")
    ggplot(mtcars, aes(x=vs, y=am)) + geom_point(colour="#00000010", stat="unique")
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
