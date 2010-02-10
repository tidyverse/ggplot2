\name{label_bquote}
\alias{label_bquote}
\title{Label facet with 'bquoted' expressions}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Create facet labels which contain the facet label in a larger expression
}
\usage{label_bquote(expr = beta ^ .(x))}
\arguments{
\item{expr}{expression to use}
}

\details{See \code{\link{bquote}} for details on the syntax of the argument.  The
label value is x.}
\seealso{\code{\link{plotmath}}}
\examples{p <- qplot(wt, mpg, data = mtcars)
p + facet_grid(~ vs, labeller = label_bquote(alpha ^ .(x)))
p + facet_grid(~ vs, labeller = label_bquote(.(x) ^ .(x)))}
\keyword{hplot}
