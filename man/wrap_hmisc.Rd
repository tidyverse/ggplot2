\name{wrap_hmisc}
\alias{mean_cl_boot}
\alias{mean_cl_normal}
\alias{mean_sdl}
\alias{median_hilow}
\alias{wrap_hmisc}
\title{Wrap up a selection of summary functions from Hmisc to make it easy to use
with \code{\link{stat_summary}}}
\usage{
  wrap_hmisc(fun)
}
\arguments{
  \item{fun}{Hmisc function to be applied. See the aliases
  below for the list of candidate functions.}
}
\description{
  See the Hmisc documentation for details of their options.
}
\seealso{
  \code{\link[Hmisc]{smean.cl.boot}},
  \code{\link[Hmisc]{smean.cl.normal}},
  \code{\link[Hmisc]{smean.sdl}},
  \code{\link[Hmisc]{smedian.hilow}}
}

