\name{theme_update}
\alias{theme_get}
\alias{theme_set}
\alias{theme_update}
\title{Get, set and update themes.}
\usage{
  theme_update(...)

  theme_get()

  theme_set(new)
}
\arguments{
  \item{...}{named list of theme settings}

  \item{new}{new theme (a list of theme elements)}
}
\description{
  Use \code{theme_update} to modify a small number of
  elements of the current theme or use \code{theme_set} to
  completely override it.
}
\examples{
qplot(mpg, wt, data = mtcars)
old <- theme_set(theme_bw())
qplot(mpg, wt, data = mtcars)
theme_set(old)
qplot(mpg, wt, data = mtcars)

old <- theme_update(panel.background = theme_rect(colour = "pink"))
qplot(mpg, wt, data = mtcars)
theme_set(old)
theme_get()

qplot(mpg, wt, data=mtcars, colour=mpg) +
  opts(legend.position=c(0.95, 0.95), legend.justification = c(1, 1))
last_plot() +
 opts(legend.background = theme_rect(fill = "white", col="white", size =3))
}

