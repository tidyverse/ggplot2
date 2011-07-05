\name{geom_rug}
\alias{geom_rug}
\title{Marginal rug plots.}
\usage{
  geom_rug(mapping = NULL, data = NULL, stat = "identity",
  position = "identity", ...)
}
\description{
  Marginal rug plots.
}
\examples{
p <- ggplot(mtcars, aes(x=wt, y=mpg))
p + geom_point()
p + geom_point() + geom_rug()
p + geom_point() + geom_rug(position='jitter')
}

