\name{label_value}
\alias{label_value}
\title{Label facets with their value.}
\usage{label_value(variable, value)}

\description{
  Label facets with their value. This is the default
  labelling scheme.
}
\arguments{
  \item{variable}{variable name passed in by facetter}
  \item{value}{variable value passed in by facetter}
}
\examples{p <- qplot(wt, mpg, data = mtcars)
p + facet_grid(~ cyl)
p + facet_grid(~ cyl, labeller = label_value)}
