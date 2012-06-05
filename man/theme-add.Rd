\name{+.theme}
\alias{+.theme}
\title{Modify a theme object}
\usage{
  +.theme(e1, e2)
}
\arguments{
  \item{e1}{theme object}

  \item{e2}{theme object to add}
}
\description{
  Modify a theme object
}
\examples{
mytheme <- theme_grey() + opts(axis.title.x = element_text(family = "Times"))
}

