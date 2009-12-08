\name{guide_legends}
\alias{guide_legends}
\alias{build_legend}
\alias{build_legend_data}
\title{Build all legend grob}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Build legends, merging where possible
}
\usage{guide_legends(scales, layers, default_mapping, theme)}
\arguments{
\item{scales}{}
\item{layers}{}
\item{default_mapping}{}
\item{theme}{}
}
\value{A list of grobs}


\examples{theme_update(legend.background = theme_rect(size = 0.2))
qplot(mpg, wt, data = mtcars)
qplot(mpg, wt, data = mtcars, colour = cyl)

# Legend with should expand to fit name
qplot(mpg, wt, data = mtcars, colour = factor(cyl))

qplot(mpg, wt, data = mtcars, colour = cyl) +
opts(legend.position = c(0.5, 0.5), 
legend.background = theme_rect(fill = "white", col = NA))

mtcars$cyl2 <- factor(mtcars$cyl, 
labels = c("a", "loooooooooooong", "two\nlines"))
qplot(mpg, wt, data = mtcars, colour = cyl2)
theme_set(theme_grey())}
\keyword{internal}
