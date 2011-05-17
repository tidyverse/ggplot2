\name{stat_unique}
\alias{stat_unique}
\title{Remove duplicates.}

\description{
  Remove duplicates.
}
\examples{ggplot(mtcars, aes(vs, am)) + geom_point(alpha = 0.1)
ggplot(mtcars, aes(vs, am)) + geom_point(alpha = 0.1, stat="unique")}
