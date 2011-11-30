\name{geom_blank}
\alias{geom_blank}
\title{Blank, draws nothing.}
\usage{
  geom_blank(mapping = NULL, data = NULL,
    stat = "identity", position = "identity", ...)
}
\description{
  The blank geom draws nothing, but can be a useful way of
  ensuring common scales between different plots.
}
\examples{
qplot(length, rating, data=movies, geom="blank")
# Nothing to see here!
}

