\name{qplot}
\alias{qplot}
\alias{quickplot}
\title{Quick plot.}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Quick plot is a convenient wrapper function for creating simple ggplot plot objects.
}
\usage{qplot(x, y = NULL, z=NULL, ..., data, facets = . ~ ., margins=FALSE, geom = "auto", stat=list(NULL), position=list(NULL), xlim = c(NA, NA), ylim = c(NA, NA), log = "", main = NULL, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), asp = NA)}
\arguments{
\item{x}{x values}
\item{y}{y values}
\item{z}{z values}
\item{...}{other arguments passed on to the geom functions}
\item{data}{data frame to use (optional)}
\item{facets}{faceting formula to use}
\item{margins}{whether or not margins will be displayed}
\item{geom}{geom to use (can be a vector of multiple names)}
\item{stat}{statistic to use (can be a vector of multiple names)}
\item{position}{position adjustment to use (can be a vector of multiple names)}
\item{xlim}{limits for x axis (aesthetics to range of data)}
\item{ylim}{limits for y axis (aesthetics to range of data)}
\item{log}{which variables to log transform ("x", "y", or "xy")}
\item{main}{character vector or expression for plot title}
\item{xlab}{character vector or expression for x axis label}
\item{ylab}{character vector or expression for y axis label}
\item{asp}{the y/x aspect ratio}
}

\details{You can use it like you'd use the \code{\link{plot}} function.}

\examples{# Use data from data.frame
qplot(mpg, wt, data=mtcars)
qplot(mpg, wt, data=mtcars, colour=cyl)
qplot(mpg, wt, data=mtcars, size=cyl)
qplot(mpg, wt, data=mtcars, facets=vs ~ am)

# Use data from local environment
attach(mtcars)
qplot(hp, wt)
qplot(hp, wt, colour=cyl)
qplot(hp, wt, size=cyl)
qplot(hp, wt, facets=vs ~ am)

qplot(1:10, rnorm(10), colour = runif(10))
qplot(1:10, letters[1:10])
mod <- lm(mpg ~ wt, data=mtcars)
qplot(resid(mod), fitted(mod))
qplot(resid(mod), fitted(mod), facets = . ~ vs)

f <- function() {
a <- 1:10
b <- a ^ 2
qplot(a, b)
} 
f()

# qplot will attempt to guess what geom you want depending on the input
# both x and y supplied = scatterplot
qplot(mpg, wt, data = mtcars)
# just x supplied = histogram
qplot(mpg, data = mtcars)
# just y supplied = scatterplot, with x = seq_along(y)
qplot(y = mpg, data = mtcars)

# Use different geoms
qplot(mpg, wt, geom="path")
qplot(factor(cyl), wt, geom=c("boxplot", "jitter"))}
\keyword{hplot}
