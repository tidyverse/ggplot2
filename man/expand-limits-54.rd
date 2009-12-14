\name{expand_limits}
\alias{expand_limits}
\title{Expand the plot limits with data.}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Some times you may want to ensure limits include a single value, for all
}
\usage{expand_limits(...)}
\arguments{
\item{...}{}
}



\examples{p <- qplot(mpg, wt, data = mtcars)
p + expand_limits(x = 0)
p + expand_limits(y = c(1, 9))
p + expand_limits(x = 0, y = 0)

qplot(mpg, wt, data = mtcars, colour = cyl) + 
expand_limits(colour = seq(2, 10, by = 2))
qplot(mpg, wt, data = mtcars, colour = factor(cyl)) + 
expand_limits(colour = factor(seq(2, 10, by = 2)))}

