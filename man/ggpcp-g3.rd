\name{ggpcp}
\alias{ggpcp}
\title{Parallel coordinates plot.}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Generate a plot ``template'' for a parallel coordinates plot.
}
\usage{ggpcp(data, vars=names(data), scale="range", ...)}
\arguments{
\item{data}{data frame}
\item{vars}{variables to include in parallel coordinates plot}
\item{scale}{scaling function, one of "range", "var" or "I"}
\item{...}{other arguments passed on plot creation}
}

\details{One way to think about a parallel coordinates plot, is as plotting
the data after it has transformation been transformed to gain a new
variable.  This function does this using \code{\link[reshape]{melt}}.

This gives us enormous flexibility as we have separated out the
type of drawing (lines by tradition) and can now use any of the existing
geom functions.  In particular this makes it very easy to create parallel
boxplots, as shown in the example.}

\examples{ggpcp(mtcars) + geom_line()
ggpcp(mtcars, scale="var") + geom_line()
ggpcp(mtcars, vars=names(mtcars)[3:6], formula= . ~cyl, scale="I") + geom_line()
ggpcp(mtcars, scale="I") + geom_boxplot(aes(group=variable))
ggpcp(mtcars, vars=names(mtcars[2:6])) + geom_line()
p <- ggpcp(mtcars, vars=names(mtcars[2:6]))
p + geom_line()
p + geom_line(aes(colour=mpg)) }
\keyword{hplot}
