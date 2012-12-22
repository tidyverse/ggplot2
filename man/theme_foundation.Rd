\name{theme_foundation}
\alias{theme_foundation}
\title{Foundational Theme}
\usage{
  theme_foundation(base_size = 12, base_family = "",
    use_sizes = TRUE)
}
\arguments{
  \item{base_size}{Base font size.}

  \item{base_family}{Base font family.}

  \item{use_sizes}{If \code{TRUE}, then define sizes and
  locations with reasonable defaults taken from
  \code{\link{theme_gray}}.}
}
\description{
  This theme is designed to be a foundation from which to
  build new themes, and not meant to be used directly.
  \code{theme_foundation} is a complete theme with only
  minimal number of elements defined.
}
\details{
  It is easier to create new themes by extending this one
  rather than \code{theme_gray} or \code{theme_bw}, because
  those themes those themes define elements deep in the
  hierarchy.
}
\examples{
# Simple theme with blue text and gray background
new_theme <- (theme_foundation()
 + theme(text = element_text(colour = "blue"),
         plot.background = element_rect("gray")))
DF <- data.frame(x = rnorm(400))
m <- ggplot(DF, aes(x = x)) + geom_histogram()
m + new_theme
}

