\name{scale_x_datetime}
\alias{scale_x_datetime}
\alias{scale_y_datetime}
\title{Position scale, date}
\usage{
  scale_x_datetime(..., expand = c(0.05, 0))

  scale_y_datetime(..., expand = c(0.05, 0))
}
\arguments{
  \item{...}{common continuous scale parameters:
  \code{name}, \code{breaks}, \code{labels},
  \code{na.value}, \code{limits} and \code{trans}.  See
  \code{\link{continuous_scale}} for more details}

  \item{expand}{a numeric vector of length two giving
  multiplicative and additive expansion constants. These
  constants ensure that the data is placed some distance
  away from the axes.}
}
\description{
  Position scale, date
}
\examples{
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
last_plot() + scale_x_datetime(breaks = date_breaks("2 weeks"))
last_plot() + scale_x_datetime(breaks = date_breaks("10 days"))
last_plot() + scale_x_datetime(breaks = date_breaks("10 days"),
  labels = date_format("\%d/\%m"))
}
\seealso{
  Other position scales: \code{\link{scale_x_continuous}},
  \code{\link{scale_x_date}},
  \code{\link{scale_x_discrete}},
  \code{\link{scale_x_log10}},
  \code{\link{scale_x_reverse}},
  \code{\link{scale_x_sqrt}},
  \code{\link{scale_y_continuous}},
  \code{\link{scale_y_date}},
  \code{\link{scale_y_discrete}},
  \code{\link{scale_y_log10}},
  \code{\link{scale_y_reverse}}, \code{\link{scale_y_sqrt}}
}

