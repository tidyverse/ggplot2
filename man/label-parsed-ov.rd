\name{label_parsed}
\alias{label_parsed}
\title{Label facets with parsed label.}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Parses the facet label, as if
}
\usage{label_parsed(variable, value)}
\arguments{
\item{variable}{}
\item{value}{}
}

\details{}
\seealso{\code{\link{plotmath}}}
\examples{mtcars$cyl2 <- factor(mtcars$cyl, labels = c("alpha", "beta", "gamma"))
qplot(wt, mpg, data = mtcars) + facet_grid(. ~ cyl2)
qplot(wt, mpg, data = mtcars) + facet_grid(. ~ cyl2, 
labeller = label_parsed)}
\keyword{hplot}
