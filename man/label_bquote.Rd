\name{label_bquote}
\alias{label_bquote}
\title{Label facet with 'bquoted' expressions...}
\usage{
  label_bquote(expr = beta^.(x))
}
\arguments{
  \item{expr}{labelling expression to use}
}
\description{
  Label facet with 'bquoted' expressions
}
\details{
  See \code{\link{bquote}} for details on the syntax of the
  argument.  The label value is x.
}
\examples{
p <- qplot(wt, mpg, data = mtcars)
p + facet_grid(. ~ vs, labeller = label_bquote(alpha ^ .(x)))
p + facet_grid(. ~ vs, labeller = label_bquote(.(x) ^ .(x)))
}
\seealso{
  \code{\link{plotmath}}
}

