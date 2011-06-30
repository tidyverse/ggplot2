\name{scale_alpha_continuous}
\title{Alpha scale for continuous variable.}
\usage{scale_alpha_continuous(..., range = c(0.1, 1))}

\description{
  Alpha scale for continuous variable.
}
\alias{scale_alpha}
\alias{scale_alpha_continuous}
\arguments{
  \item{...}{Other arguments passed on to \code{\link{continuous_scale}} 
to control name, limits, breaks, labels and so forth.}
  \item{x}{range of output alpha values.  Should lie between 0 and 1.}
}
\examples{(p <- qplot(mpg, cyl, data=mtcars, alpha=cyl))
p + scale_alpha("cylinders")
p + scale_alpha("number\\nof\\ncylinders")

p + scale_alpha(range = c(0.4, 0.8))}

