\name{set_default_scale}
\alias{set_default_scale}
\title{Set default scale}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Overrides the default scale with one of your choosing.
}
\usage{set_default_scale(aesthetic, type, scale, ...)}
\arguments{
\item{aesthetic}{type of variable (discrete, continuous, date)}
\item{type}{name of new default scale}
\item{scale}{}
\item{...}{}
}



\examples{qplot(mpg, wt, data=mtcars, colour=factor(cyl)) 
set_default_scale("colour","discrete", "grey")
qplot(mpg, wt, data=mtcars, colour=factor(cyl)) 
set_default_scale("colour","discrete", "hue")}
\keyword{internal}
