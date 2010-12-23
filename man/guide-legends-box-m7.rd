\name{guide_legends_box}
\alias{guide_legends_box}
\title{Legends}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Create and arrange legends for all scales.
}
\usage{guide_legends_box(scales, layers, default_mapping, horizontal = FALSE, theme)}
\arguments{
\item{scales}{scales object}
\item{layers}{direction of scales, vertical by default}
\item{default_mapping}{}
\item{horizontal}{}
\item{theme}{}
}
\value{frameGrob, or NULL if no legends}
\details{This function gathers together all of the legends produced by
the scales that make up the plot and organises them into a
\code{\link[grid]{frameGrob}}.

If there are no legends to create, this function will return \code{NULL}}

\examples{theme_update(legend.background = theme_rect(size = 0.2))
mtcars$long <- factor(sample(3, nrow(mtcars), TRUE),
labels = c("this is very long label", "this is very long label2", "this is\nvery long\nlabel3"))
mtcars$short_elements_with_long_title <- factor(sample(2, nrow(mtcars), TRUE), labels = c("s1", "s2"))

# with short title and long key/values
p <- qplot(mpg, wt, data = mtcars, colour = factor(cyl), shape = long)
p
p + opts(legend.direction = "horizontal", legend.position = "bottom")
p + opts(legend.direction = "horizontal", legend.position = "bottom", legend.box = "vertical")

# with long title and short key/values
p <- qplot(mpg, wt, data = mtcars, colour = factor(cyl), shape = short_elements_with_long_title)
p
p + opts(legend.direction = "horizontal", legend.position = "bottom") # to be fixed
p + opts(legend.direction = "horizontal", legend.position = "bottom", legend.box = "vertical")
theme_set(theme_grey())}
\keyword{hplot}
\keyword{internal}
