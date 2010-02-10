\name{ylim}
\alias{ylim}
\title{Set y limits}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Convenience function to set the limits of the y axis.
}
\usage{ylim(...)}
\arguments{
\item{...}{if numeric, will create a continuos scale, if factor or character, will create a discrete scale}
\item{}{limits}
}



\examples{ylim(15, 20)
ylim(c(10, 20))
ylim("a", "b", "c") 
qplot(mpg, wt, data=mtcars) + ylim(15, 20)}
\keyword{hplot}
