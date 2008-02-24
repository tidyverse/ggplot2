\name{aes}
\alias{aes}
\alias{str.uneval}
\alias{print.uneval}
\title{dput(c("group","order", sort(unique(unlist(sapply(Geom$find_all(), function(y) c(names(y$default_aes()), y$required_aes)))))))}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Generate aesthetic mappings
}
\usage{aes(...)}
\arguments{
\item{...}{List of name value pairs}
\item{}{List of name value pairs}
\item{}{values from aesthetic mappings}
\item{}{defaults}
\item{}{user specified values}
\item{}{numeric vector}
}
\value{a data.frame, with all factors converted to character strings}
\details{aes creates a list of unevaluated expressions.  This function also performs
partial name matching, converts color to colour, and old style R names to
new ggplot names (eg. pch to shape, cex to size)

@arguments List of name value pairs
@keyword internal
@alias str.uneval
@alias print.uneval
@seealso \code{\link{aes_string}}
X aes(x = mpg, y = wt)
X aes(x = mpg ^ 2, y = wt / cyl)
Generate aesthetic mappings from a string
Aesthetic mappings describe how variables in the data are mapped to visual properties (aesthetics) of geoms.  Compared to aes this function operates on strings rather than expressions.

\code{aes_string} is particularly useful when writing functions that create
plots because you can use strings to define the aesthetic mappings, rather
than having to mess around with expressions.

@arguments List of name value pairs
@keyword internal
@seealso \code{\link{aes}}
X aes_string(x = "mpg", y = "wt")
X aes(x = mpg, y = wt)
Aesthetic defaults
Convenience method for setting aesthetic defaults

@arguments values from aesthetic mappings
@arguments defaults
@arguments user specified values
@value a data.frame, with all factors converted to character strings
@keyword internal
Resolution
Compute the "resolution" of a data vector, ie. what is the smallest non-zero
distance between adjacent values.}
\seealso{
 \item{\code{\link{aes_string}}}
 \item{\code{\link{aes}}}
}
\examples{aes(x = mpg, y = wt)
aes(x = mpg ^ 2, y = wt / cyl)
aes_string(x = "mpg", y = "wt")
aes(x = mpg, y = wt)}
\keyword{internal}
\keyword{internal}
\keyword{internal}
\keyword{hplot}
\keyword{internal}
