\name{label_both}
\alias{label_both}
\title{Label facets with value and variable.}
\usage{label_both(variable, value)}

\description{
  Label facets with value and variable.
}
\arguments{
  \item{variable}{variable name passed in by facetter}
  \item{value}{variable value passed in by facetter}
}
\examples{p <- qplot(wt, mpg, data = mtcars)
p + facet_grid(~ cyl)
p + facet_grid(~ cyl, labeller = label_both)}
