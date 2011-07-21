\name{aes_string}
\alias{aes_string}
\title{Generate aesthetic mappings from a string}
\usage{
  aes_string(...)
}
\arguments{
  \item{...}{List of name value pairs}
}
\description{
  Aesthetic mappings describe how variables in the data are
  mapped to visual properties (aesthetics) of geoms.
  Compared to aes this function operates on strings rather
  than expressions.
}
\details{
  \code{aes_string} is particularly useful when writing
  functions that create plots because you can use strings
  to define the aesthetic mappings, rather than having to
  mess around with expressions.
}
\examples{
aes_string(x = "mpg", y = "wt")
aes(x = mpg, y = wt)
}
\seealso{
  \code{\link{aes}}
}

