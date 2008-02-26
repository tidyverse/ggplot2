\name{set_default_scale}
\alias{set_default_scale}
\title{Set default scale}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Overrides the default scale with one of your choosing.
}
\usage{set_default_scale(aesthetic, type, scale, ...)}
\arguments{
\item{aesthetic}{}
\item{type}{}
\item{scale}{}
\item{...}{}
}

\details{}

\examples{qplot(mpg, wt, data=mtcars, colour=factor(cyl)) 
set_default_scale("colour","discrete", "grey")
qplot(mpg, wt, data=mtcars, colour=factor(cyl)) 
set_default_scale("colour","discrete", "hue")}
\keyword{internal}
