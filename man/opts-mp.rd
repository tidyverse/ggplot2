\name{opts}
\alias{opts}
\title{Plot options}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Set options/theme elements for a single plot
}
\usage{opts(...)}
\arguments{
\item{...}{named list of theme settings}
}

\details{Use this function if you want to modify a few theme settings for
a single plot.}

\examples{p <- qplot(mpg, wt, data = mtcars)
p 
p + opts(panel_background = theme_rect(colour = "pink"))
p + theme_bw()}

