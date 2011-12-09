\name{scale_linetype}
\alias{scale_linetype}
\alias{scale_linetype_continuous}
\alias{scale_linetype_discrete}
\title{Scale for line patterns.}
\usage{
  scale_linetype(...)

  scale_linetype_continuous(...)

  scale_linetype_discrete(...)
}
\description{
  Default line types based on a set supplied by Richard
  Pearson, University of Manchester.  Line types can not be
  mapped to continuous values.
}
\examples{
library(reshape2)
library(plyr)
ecm <- melt(economics, id = "date")
rescale01 <- function(x) (x - min(x)) / diff(range(x))
ecm <- ddply(ecm, "variable", mutate, value = rescale01(value))

qplot(date, value, data=ecm, geom="line", group=variable)
qplot(date, value, data=ecm, geom="line", linetype=variable)
qplot(date, value, data=ecm, geom="line", colour=variable)

# See scale_manual for more flexibility
}

