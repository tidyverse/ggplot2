\name{geom_blank}
\alias{geom_blank}
\title{Blank, draws nothing.}

\description{
  Blank, draws nothing.
}

\details{
  The blank geom draws nothing, but can be a useful way of
  ensuring common scales between different plots.
}
\examples{qplot(length, rating, data=movies, geom="blank")
# Nothing to see here!}
