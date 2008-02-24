\name{check_required_aesthetics}
\alias{check_required_aesthetics}
\title{Check required aesthetics are present}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
This is used by geoms and stats to give a more helpful error message
}
\usage{check_required_aesthetics(required, present, name)}
\arguments{
\item{required}{character vector of required aesthetics}
\item{present}{character vector of present aesthetics}
\item{name}{list to concatenate}
\item{}{character vectors to be concatenated}
\item{}{default separator}
\item{}{default collapser}
\item{}{function to call}
\item{}{parameter names of function}
\item{}{data.frame}
\item{}{suppress warning that rows are being removed?}
\item{}{optional function name to make warning message more informative}
}

\details{@arguments character vector of required aesthetics
@arguments character vector of present aesthetics
@argument name of object for error message
@keyword internal
Concatenate a named list for output
Print a \code{list(a=1, b=2)} as \code{(a=1, b=2)}

@arguments list to concatenate
X clist(list(a=1, b=2))
X clist(par()[1:5])
Abbreviated paste
Alias for paste with a shorter name and convenient defaults

@arguments character vectors to be concatenated
@arguments default separator
@arguments default collapser
@keyword internal
Quietly try to require a package
Queitly require a package, returning an error message if that package is not installed.

@argument name of package
@keyword internal
Return unique columns
This is used for figuring out which columns are constant within a group

@keyword internal
A "safe" version of do.call
\code{safe.call} works like \code{\link{do.call}} but it will only supply arguments that exist in the function specification.

If ... is present in the param list, all parameters will be passed through
unless \code{ignore.dots = TRUE}.  Positional arguments are not currently
supported.

@arguments function to call
@arugments named list of parameters to be supplied to function
@arguments parameter names of function
@arguments
Convenience function to remove missing values from a data.frame
Remove all non-complete rows, with a warning if \code{na.rm = FALSE}.

ggplot is somewhat more accomodating of missing values than R generally.
For those stats which require complete data, missing values will be
automatically removed with a warning.  If \code{na.rm = TRUE} is supplied
to the statistic, the warning will be suppressed.

@arguments data.frame
@arguments suppress warning that rows are being removed?
@argumnets variables to check for missings in
@arguments optional function name to make warning message more informative
@keyword internal
X a <- remove_missing(movies)
X a <- remove_missing(movies, na.rm = TRUE)
X qplot(mpaa, budget, data=movies, geom="boxplot")
Traceback alias
Alias of traceback with fewer keypresses, and severe restriction on number of lines for each function

@keyword manip
@keyword internal
Rescale numeric vector
Rescale numeric vector to have specified minimum and maximum.
If vector has length one, it is not rescaled, but is restricted to the range.}

\examples{clist(list(a=1, b=2))
clist(par()[1:5])
a <- remove_missing(movies)
a <- remove_missing(movies, na.rm = TRUE)
qplot(mpaa, budget, data=movies, geom="boxplot")}
\keyword{internal}
\keyword{internal}
\keyword{internal}
\keyword{internal}
\keyword{internal}
\keyword{manip}
\keyword{internal}
\keyword{manip}
