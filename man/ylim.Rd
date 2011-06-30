\name{ylim}
\alias{ylim}
\title{Convenience function to set the limits of the y axis.}
\usage{ylim(...)}

\description{
  Convenience function to set the limits of the y axis.
}
\arguments{
  \item{...}{if numeric, will create a continuos scale, if factor or
character, will create a discrete scale}
}
\examples{ylim(15, 20)
ylim(c(10, 20))
ylim("a", "b", "c") 
qplot(mpg, wt, data=mtcars) + ylim(15, 20)}

