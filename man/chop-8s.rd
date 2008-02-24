\name{chop}
\alias{chop}
\title{Chop}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Chop a continuous variable into a categorical variable.
}
\usage{chop(x, n=5, method="quantiles", midpoint=0, digits=2)}
\arguments{
\item{x}{}
\item{n}{}
\item{method}{}
\item{midpoint}{}
\item{digits}{}
}

\details{Chop provides a convenient interface to the main methods of
converting a continuous variable into a categorical variable.

@argument continuous variable to chop into pieces
@argument number of bins to chop into
@argument method to use: quantiles (approximately equal numbers), cut (equal lengths) or pretty
@argument mid point for diverging factors
@seealso \code{\link{chop.breaks}} to get breaks used
@keyword manip
Chop breaks
Calculate breakpoints for chop function

@argument continuous variable
@argument number of bins to chop into
@argument method to use: quantiles (approximately equal numbers), cut (equal lengths) or pretty
@argument mid point for diverging factors
@keyword manip
@keyword internal
Automatic chop
Keep categorical variables as is, chop up continuous variable}
\seealso{\code{\link{chop.breaks}} to get breaks used}
\examples{}
\keyword{manip}
\keyword{manip}
\keyword{internal}
\keyword{manip}
\keyword{internal}
