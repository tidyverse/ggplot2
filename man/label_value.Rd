\name{label_value}
\alias{label_value}
\title{Label facets with their value.
This is the default labelling scheme.}
\usage{
  label_value(variable, value)
}
\arguments{
  \item{variable}{variable name passed in by facetter}

  \item{value}{variable value passed in by facetter}
}
\description{
  Label facets with their value. This is the default
  labelling scheme.
}
\examples{
p <- qplot(wt, mpg, data = mtcars)
p + facet_grid(. ~ cyl)
p + facet_grid(. ~ cyl, labeller = label_value)
}
\seealso{
  Other facet labellers: \code{\link{label_both}},
  \code{\link{label_bquote}}, \code{\link{label_parsed}}
}

