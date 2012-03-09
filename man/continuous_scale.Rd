\name{continuous_scale}
\alias{continuous_scale}
\title{Continuous scale constructor.}
\usage{
  continuous_scale(aesthetics, scale_name, palette,
    name = NULL, breaks = waiver(),
    minor_breaks = waiver(), labels = waiver(),
    legend = NULL, limits = NULL, rescaler = rescale,
    oob = censor, expand = c(0, 0), na.value = NA,
    trans = "identity", guide = "legend")
}
\arguments{
  \item{aesthetics}{character}

  \item{trans}{A transformation object, as created by
  \code{\link[scales]{trans_new}}.  You can also give the
  name of the transformer: e.g. instead of
  \code{log2_trans()} you can use \code{"log2"}.}
}
\description{
  Continuous scale constructor.
}
\keyword{internal}

