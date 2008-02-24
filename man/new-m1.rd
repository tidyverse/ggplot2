\name{new}
\alias{new}
\title{time <- ScaleTime$new(major="months", minor="weeks")}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
For an time axis (ie. given two dates indicating the start and end of the time series), you want to be able to specify:
}
\usage{new(., major=NULL, minor=NULL, format=NULL, variable="x", name=NULL)}
\arguments{
\item{.}{}
\item{major}{}
\item{minor}{}
\item{format}{}
\item{variable}{}
\item{name}{}
}

\details{* the interval between major and minor ticks. A string (second, minute, hour, day, week, month, quarter, year + all plurals) posibly including multiplier (usually integer, always positive) giving the interval between ticks.

* the position of the first tick, as a date/time.  This should default to a round number of intervals, before first the data point if necessary.

* format string which controls how the date is printed (should default to displaying just enough to distinguish each interval).}

\examples{}

