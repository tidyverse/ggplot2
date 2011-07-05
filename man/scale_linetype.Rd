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
  Scale for line patterns.
}
\details{
  Default line types based on a set supplied by Richard
  Pearson, University of Manchester.  Line types can not be
  mapped to continuous values.
}
\examples{
ec_scaled <- data.frame(
date = economics$date,
rescaler(economics[, -(1:2)], "range")
)
ecm <- melt(ec_scaled, id = "date")

qplot(date, value, data=ecm, geom="line", group=variable)
qplot(date, value, data=ecm, geom="line", linetype=variable)
qplot(date, value, data=ecm, geom="line", colour=variable)

# See scale_manual for more flexibility
}

