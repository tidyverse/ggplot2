\name{scale_datetime}
\alias{scale_datetime}
\alias{scale_x_datetime}
\alias{scale_y_datetime}
\alias{ScaleDatetime}
\title{scale\_datetime}
\description{Position scale, date time}
\details{
This page describes scale\_datetime, see \code{\link{layer}} and \code{\link{qplot}} for how to create a complete plot from individual components.
}
\usage{scale_x_datetime(name=NULL, limits=NULL, major=NULL, minor=NULL, format=NULL, tz="", ...)
scale_y_datetime(name=NULL, limits=NULL, major=NULL, minor=NULL, format=NULL, tz="", ...)}
\arguments{
 \item{name}{name of scale to appear in legend or on axis.  Maybe be an expression: see ?plotmath}
 \item{limits}{numeric vector of length 2, giving the extent of the scale}
 \item{major}{NULL}
 \item{minor}{NULL}
 \item{format}{NULL}
 \item{tz}{NULL}
 \item{...}{ignored }
}
\seealso{\itemize{
  \item \code{\link{scale_discrete}}: Discrete position scales
  \item \url{http://had.co.nz/ggplot2/scale_datetime.html}
}}
\value{A \code{\link{layer}}}
\examples{\dontrun{
start <- ISOdate(2001, 1, 1, tz = "")
df <- data.frame(
  day30  = start + round(runif(100, max = 30 * 86400)),
  day7  = start + round(runif(100, max = 7 * 86400)),
  day   = start + round(runif(100, max = 86400)),
  hour10 = start + round(runif(100, max = 10 * 3600)),
  hour5 = start + round(runif(100, max = 5 * 3600)),
  hour  = start + round(runif(100, max = 3600)),
  min10 = start + round(runif(100, max = 10 * 60)),
  min5  = start + round(runif(100, max = 5 * 60)),
  min   = start + round(runif(100, max = 60)),
  sec10 = start + round(runif(100, max = 10)),
  y = runif(100)
)

# Automatic scale selection
qplot(sec10, y, data = df)
qplot(min, y, data = df)
qplot(min5, y, data = df)
qplot(min10, y, data = df)
qplot(hour, y, data = df)
qplot(hour5, y, data = df)
qplot(hour10, y, data = df)
qplot(day, y, data = df)
qplot(day30, y, data = df)

# Manual scale selection
qplot(day30, y, data = df)
last_plot() + scale_x_datetime(major = "2 weeks")
last_plot() + scale_x_datetime(major = "2 weeks", minor = "1 week")
last_plot() + scale_x_datetime(major = "10 days")
# See ?strptime for formatting parameters
last_plot() + scale_x_datetime(major = "10 days", format = "%d/%m")

}}
\author{Hadley Wickham, \url{http://had.co.nz/}}
\keyword{hplot}
