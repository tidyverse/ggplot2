\name{qplot}
\alias{qplot}
\alias{qplot}
\title{Quick plot.}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Quick plot is a convenient wrapper function for creating simple ggplot plot objects.
}
\usage{qplot(x, y = NULL, z=NULL, ..., data, facets = . ~ ., margins=FALSE, geom = "point", stat=list(NULL), position=list(NULL), xlim = c(NA, NA), ylim = c(NA, NA), log = "", main = NULL, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)))}
\arguments{
\item{x}{x values}
\item{y}{y values}
\item{z}{z values}
\item{...}{other arguments passed on to the geom functions}
\item{data}{data frame to use (optional)}
\item{facets}{facetting formula to use}
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
}

\details{\code{qplot} provides a quick way to create simple plots.}

\examples{# Use data from data.frame
qplot(mpg, wt, data=mtcars)
qplot(mpg, wt, data=mtcars, colour=cyl)
qplot(mpg, wt, data=mtcars, size=cyl)
qplot(mpg, wt, data=mtcars, facets=vs ~ am)

# Use data from workspace environment
attach(mtcars)
qplot(mpg, wt)
qplot(mpg, wt, colour=cyl)
qplot(mpg, wt, size=cyl)
qplot(mpg, wt, facets=vs ~ am)

# Use different geoms
qplot(mpg, wt, geom="path")
qplot(factor(cyl), wt, geom=c("boxplot", "jitter"))}
\keyword{hplot}
