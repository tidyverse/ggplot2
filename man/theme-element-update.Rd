\name{|.theme}
\alias{|.theme}
\title{Modify properties of an element in a theme object}
\usage{
  |.theme(t1, t2)
}
\arguments{
  \item{e1}{theme object}

  \item{e2}{theme object to use for updating}
}
\description{
  In contrast with the \code{+.theme} operator, this
  operator does not replace the entire element; it only
  updates element properties which are present (not NULL)
  in the second object.
}
\examples{
# Compare these results
mod_el <- theme_grey() | opts(text = element_text(family = "Times"))
add_el <- theme_grey() + opts(text = element_text(family = "Times"))

mod_el$text
add_el$text
}

