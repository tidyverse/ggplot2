\name{scale_alpha_continuous}
\title{Alpha scale for continuous variable.}
\usage{scale_alpha_continuous(..., range=c(0.1, 1))}

\description{
  Alpha scale for continuous variable.
}
\alias{scale_alpha}
\alias{scale_alpha_continuous}
\examples{(p <- qplot(mpg, cyl, data=mtcars, alpha=cyl))
p + scale_alpha("cylinders")
p + scale_alpha("number\\nof\\ncylinders")

p + scale_alpha(to = c(0.4, 0.8))}
