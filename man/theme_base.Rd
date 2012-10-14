\name{theme_base0}
\alias{theme_base}
\alias{theme_base0}
\alias{theme_base_sizes}
\title{Base themes}
\usage{
  theme_base0(base_size = 12, base_family = "")

  theme_base_sizes()

  theme_base(base_size = 12, base_family = "")
}
\arguments{
  \item{base_size}{Base font size.}

  \item{base_family}{Base font family.}
}
\description{
  These themes are not meant to be used directly. Instead,
  they should be used as themes from which to build a
  themes.  These themes are better suited to extension than
  \code{theme_gray} or \code{theme_bw} because those themes
  define elements deep in the hierarchy.
}
\details{
  \code{theme_base0} is a complete theme with only
  top-level elements defined. \code{theme_base_sizes} is an
  incomplete theme with some the non-color elements of
  \code{theme_gray}, e.g. relative size and positions of
  the text elements. \code{theme_base} combines
  \code{theme_base0} and \code{theme_base_sizes}; this is
  like \code{theme_gray} without any of the color
  information.
}

