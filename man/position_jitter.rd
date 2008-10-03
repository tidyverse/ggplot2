\name{position_jitter}
\alias{position_jitter}
\alias{PositionJitter}
\title{position\_jitter}
\description{Jitter points to avoid overplotting}
\details{
This page describes position\_jitter, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{position_jitter(width=NULL, height=NULL, ...)}
\arguments{
 \item{width}{degree of jitter in x direction. Defaults to 40\% of the resolution of the data.}
 \item{height}{degree of jitter in y direction. Defaults to 40\% of the resolution of the data.}
 \item{...}{other arguments}
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot2/position_jitter.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
qplot(am, vs, data=mtcars)

# Default amount of jittering will generally be too much for 
# small datasets:
qplot(am, vs, data=mtcars, position="jitter")
# Control the amount as follows
qplot(am, vs, data=mtcars, position=position_jitter(w=0.1, h=0.1))

# The default works better for large datasets, where it will 
# will up as much space as a boxplot or a bar
qplot(cut, price, data=diamonds, geom=c("boxplot", "jitter"))
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
