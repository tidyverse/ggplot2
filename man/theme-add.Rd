\name{+.theme}
\alias{\%+replace\%}
\alias{+.theme}
\title{Modify elements in a theme object}
\usage{
  +.theme(e1, e2)

  %+replace%(e1, e2)
}
\arguments{
  \item{e1}{theme object}

  \item{e2}{theme object to use for updating e1}
}
\description{
  The \code{+} operator and the \code{\%+replace\%} can be
  used to modify elements in themes.
}
\details{
  The \code{+} operator completely replaces elements with
  elements from e2.

  In contrast, the \code{\%+replace\%} operator does not
  replace the entire element; it only updates element
  properties which are present (not NULL) in the second
  object.
}
\examples{
# Compare these results
add_el <- theme_grey() + theme(text = element_text(family = "Times"))
rep_el <- theme_grey() \%+replace\% theme(text = element_text(family = "Times"))

add_el$text
rep_el$text
}

