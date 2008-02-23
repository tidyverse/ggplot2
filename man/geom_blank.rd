\name{GeomBlank}
\alias{geom_blank}
\alias{GeomBlank}
\title{geom_blank}
\description{Blank, draws nothing}
\details{
This page describes \code{\link{geom_blank}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{geom_blank(...)}
\arguments{
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/geom_blank.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    qplot(length, rating, data=movies, geom="blank")
    # Nothing to see here!
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
