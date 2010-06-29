\name{position_stack}
\alias{position_stack}
\alias{PositionStack}
\title{position\_stack}
\description{Stack overlapping objects on top of one another}
\details{
This page describes position\_stack, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{position_stack(width = NULL, height = NULL, ...)}
\arguments{
 \item{width}{NULL}
 \item{height}{NULL}
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \url{http://had.co.nz/ggplot2/position_stack.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
# Stacking is the default behaviour for most area plots:
ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) + geom_bar()
  
ggplot(diamonds, aes(price)) + geom_histogram(binwidth=500)
ggplot(diamonds, aes(price, fill = cut)) + geom_histogram(binwidth=500)

# Stacking is also useful for time series
data.set <- data.frame(
  Time = c(rep(1, 4),rep(2, 4), rep(3, 4), rep(4, 4)),
  Type = rep(c('a', 'b', 'c', 'd'), 4),
  Value = rpois(16, 10)
)

qplot(Time, Value, data = data.set, fill = Type, geom = "area")
# If you want to stack lines, you need to say so:
qplot(Time, Value, data = data.set, colour = Type, geom = "line")
qplot(Time, Value, data = data.set, colour = Type, geom = "line",
  position = "stack")
# But realise that this makes it *much* harder to compare individual
# trends
}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
