\name{aesthetic_size}
\alias{aesthetic-size}
\alias{aesthetic_size}
\alias{size}
\title{Aesthetic: size}
\description{
  This page demonstrates the usage of the size aesthetic.
}
\examples{
# Size examples
# Should be specified with a numerical value (in millimetres),
# or from a variable source
p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point(size = 4)
p + geom_point(aes(size = qsec))
p + geom_point(size = 2.5) + geom_hline(yintercept = 25, size = 3.5)
}
\seealso{
  Other aesthetics: \code{\link{aesthetic_alpha}},
  \code{\link{aesthetic_colour_fill}},
  \code{\link{aesthetic_group_order}},
  \code{\link{aesthetic_linetype}},
  \code{\link{aesthetic_shape}}
}

