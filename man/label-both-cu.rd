\name{label_both}
\alias{label_both}
\title{Label facets with value and variable}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Join together facet value and the name of the variable to create a label.
}
\usage{label_both(variable, value)}
\arguments{
\item{variable}{}
\item{value}{}
}



\examples{p <- qplot(wt, mpg, data = mtcars)
p + facet_grid(~ cyl)
p + facet_grid(~ cyl, labeller = label_both)}
\keyword{hplot}
