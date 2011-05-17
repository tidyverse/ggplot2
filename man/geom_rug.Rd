\name{geom_rug}
\alias{geom_rug}
\title{Marginal rug plots.}

\description{
  Marginal rug plots.
}
\examples{p <- ggplot(mtcars, aes(x=wt, y=mpg))
p + geom_point()
p + geom_point() + geom_rug()
p + geom_point() + geom_rug(position='jitter')}
