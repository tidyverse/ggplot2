\name{xlim}
\alias{xlim}
\title{Set x limits}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Convenience function to set the limits of the x axis.
}
\usage{xlim(...)}
\arguments{
\item{...}{}
}

\details{}

\examples{xlim(15, 20)
xlim(20, 15)
xlim(c(10, 20))
xlim("a", "b", "c") 
qplot(mpg, wt, data=mtcars) + xlim(15, 20)}
\keyword{hplot}
