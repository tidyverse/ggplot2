\name{stat_bindot}
\alias{stat_bindot}
\title{Bin data for dot plot.}
\usage{
  stat_bindot(mapping = NULL, data = NULL,
    geom = "dotplot", position = "identity", width = 0.9,
    binaxis = "x", method = "dotdensity",
    binpositions = "bygroup", drop = FALSE, right = TRUE,
    na.rm = FALSE, ...)
}
\arguments{
  \item{binaxis}{The axis to bin along, "x" (default) or
  "y"}

  \item{method}{"dotdensity" (default) for dot-density
  binning, or "histodot" for fixed bin widths (like
  stat_bin)}

  \item{binwidth}{When \code{method} is "dotdensity, this
  specifies maximum bin width.  When \code{method} is
  "histodot", this specifies bin width.  Defaults to 1/30
  of the range of the data}

  \item{binpositions}{When \code{method} is "dotdensity",
  "bygroup" (default) determines positions of the bins for
  each group separately. "all" determines positions of the
  bins with all the data taken together; this is used for
  aligning dot stacks across multiple groups.}

  \item{breaks}{When \code{method} is "histodot", actual
  breaks to use.  Overrides bin width and origin}

  \item{origin}{When \code{method} is "histodot", origin of
  first bin}

  \item{width}{When \code{method} is "histodot", width of
  bars when used with categorical data}

  \item{right}{When \code{method} is "histodot", should
  intervals be closed on the right (a, b], or not [a, b)}

  \item{drop}{If TRUE, remove all bins with zero counts}
}
\value{
  New data frame with additional columns: \item{x}{center
  of each bin, if binaxis is "x"} \item{y}{center of each
  bin, if binaxis is "x"} \item{binwidth}{max width of each
  bin if method is "dotdensity"; width of each bin if
  method is "histodot"} \item{count}{number of points in
  bin} \item{ncount}{count, scaled to maximum of 1}
  \item{density}{density of points in bin, scaled to
  integrate to 1, if method is "histodot"}
  \item{ndensity}{density, scaled to maximum of 1, if
  method is "histodot"}
}
\description{
  Missing values are currently silently dropped. If weights
  are used, they must be integer values.
}
\examples{
# See geom_dotplot for examples
}

