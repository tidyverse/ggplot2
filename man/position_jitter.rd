\name{PositionJitter}
\alias{position_jitter}
\alias{PositionJitter}
\title{position_jitter}
\description{Jitter points to avoid overplotting}
\details{
This page describes \code{\link{position_jitter}}, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{position_jitter(...)}
\arguments{
 \item{...}{other arguments}
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot/position_jitter.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
    qplot(am, vs, data=mtcars)
    qplot(am, vs, data=mtcars, position="jitter")
    # Control amount of jittering by calling position_jitter
    qplot(am, vs, data=mtcars, position=position_jitter(x=10, y=0))
    qplot(am, vs, data=mtcars, position=position_jitter(x=0.5, y=0.5))
    
    # See lots of actually useful examples at geom_jitter
    # You can, however, jitter any geom, however little sense it might make
    qplot(cut, clarity, data=diamonds, geom="blank", group=1) + geom_path()
    qplot(cut, clarity, data=diamonds, geom="blank", group=1) + geom_path(position="jitter")
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
