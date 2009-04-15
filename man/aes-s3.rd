\name{aes}
\alias{aes}
\alias{str.uneval}
\alias{print.uneval}
\alias{[.uneval}
\title{Generate aesthetic mappings}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Aesthetic mappings describe how variables in the data are mapped to visual properties (aesthetics) of geoms.
}
\usage{aes(x, y, ...)}
\arguments{
\item{x}{x value}
\item{y}{y value}
\item{...}{List of name value pairs}
}

\details{aes creates a list of unevaluated expressions.  This function also performs
partial name matching, converts color to colour, and old style R names to
new ggplot names (eg. pch to shape, cex to size)}
\seealso{\code{\link{aes_string}}}
\examples{aes(x = mpg, y = wt)
aes(x = mpg ^ 2, y = wt / cyl)}
\keyword{internal}
