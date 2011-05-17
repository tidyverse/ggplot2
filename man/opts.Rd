\name{opts}
\alias{opts}
\title{Set options/theme elements for a single plot...}
\usage{opts(...)}

\description{
  Set options/theme elements for a single plot
}

\details{
  Use this function if you want to modify a few theme
  settings for a single plot.
}
\arguments{
  \item{...}{named list of theme settings}
}
\examples{p <- qplot(mpg, wt, data = mtcars)
p 
p + opts(panel_background = theme_rect(colour = "pink"))
p + theme_bw()}
