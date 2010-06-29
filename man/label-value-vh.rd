\name{label_value}
\alias{label_value}
\title{Label facets with their value}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
The default facet labelling just uses the value of the variable
}
\usage{label_value(variable, value)}
\arguments{
\item{variable}{variable name passed in by facetter}
\item{value}{variable value passed in by facetter}
}



\examples{p <- qplot(wt, mpg, data = mtcars)
p + facet_grid(~ cyl)
p + facet_grid(~ cyl, labeller = label_value)}
\keyword{hplot}
