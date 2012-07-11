\name{rel}
\alias{rel}
\title{Relative sizing for theme elements}
\usage{
  rel(x)
}
\arguments{
  \item{x}{A number representing the relative size}
}
\description{
  Relative sizing for theme elements
}
\examples{
qplot(1:3, 1:3) + theme(axis.title.x = element_text(size = rel(2.5)))
}

