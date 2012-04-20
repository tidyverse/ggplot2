\name{discrete_scale}
\alias{discrete_scale}
\title{Discrete scale constructor.}
\usage{
  discrete_scale(aesthetics, scale_name, palette,
    name = NULL, breaks = waiver(), labels = waiver(),
    legend = NULL, limits = NULL, expand = c(0, 0),
    na.value = NA, drop = TRUE, guide = "legend")
}
\arguments{
  \item{labels}{\code{NULL} for no labels, \code{waiver()}
  for default labels (labels the same as breaks), a
  character vector the same length as breaks, or a named
  character vector whose names are used to match
  replacement the labels for matching breaks.}
}
\description{
  Discrete scale constructor.
}
\keyword{internal}

