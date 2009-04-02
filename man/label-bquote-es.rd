\name{label_bquote}
\alias{label_bquote}
\title{X p <- qplot(wt, mpg, data = mtcars)}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
X p + facet_grid(~ vs + am, labeller = label_bquote(alpha ^ .(x)))
}
\usage{label_bquote(expr = beta ^ .(x))}
\arguments{
\item{expr}{}
}

\details{}

\examples{p <- qplot(wt, mpg, data = mtcars)
p + facet_grid(~ vs + am, labeller = label_bquote(alpha ^ .(x)))
p + facet_grid(~ vs + am, labeller = label_bquote(.(x) ^ .(x)))}

