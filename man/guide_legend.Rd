\name{guide_legend}
\alias{guide_legend}
\title{Legend guide}
\usage{
  guide_legend(title = waiver(), title.position = NULL,
  title.theme = NULL, label = TRUE, label.position = NULL,
  label.theme = NULL, keywidth = NULL, keyheight = NULL,
  direction = NULL, default.unit = "line", set.aes =
  list(), ...)
}
\arguments{
  \item{title}{A character string or expression indicating
  a title of guide. If \code{NULL}, the title is not shown.
  By default (\code{\link{waiver()}}), the name of the
  scale object or tha name specified in \code{\link{labs}}
  is used for the title.}

  \item{title.position}{A character string indicating the
  position of a title. One of "top" (default for a vertical
  guide), "bottom", "left" (default for a horizontal
  guide), or "right."}

  \item{title.theme}{A theme object for rendering the title
  text. Usually the object of \code{\link{theme_text}} is
  expected. By default, the theme is specified by
  \code{legend.title} in \code{\link{opts}} or theme.}

  \item{label}{logical. If \code{TRUE} then the labels are
  drawn. If \code{FALSE} then the labels are invisible.}

  \item{label.position}{A character string indicating the
  position of a label. One of "top", "bottom", "left", or
  "right" (default).}

  \item{label.theme}{A theme object for rendering the label
  text. Usually the object of \code{\link{theme_text}} is
  expected. By default, the theme is specified by
  \code{legend.text} in \code{\link{opts}} or theme.}

  \item{keywidth}{A numeric or a unit object specifying the
  width of the legend key. Default value is
  \code{legend.key.width} or \code{legend.key.size} in
  \code{\link{opts}} or theme.}

  \item{keyheight}{A numeric or a unit object specifying
  the height of the legend key. Default value is
  \code{legend.key.height} or \code{legend.key.size} in
  \code{\link{opts}} or theme.}

  \item{direction}{A character string indicating the
  direction of the guide. One of "horizontal" or
  "vertical."}

  \item{default.unit}{A character string indicating unit
  for \code{keywidth} and \code{keyheight}.}

  \item{set.aes}{A list specifying aesthetic parameters of
  legend key. See details and examples.}

  \item{...}{ignored.}
}
\value{
  Guide object
}
\description{
  Legend type guide
}
\details{
  Legend type guide shows key (i.e., geoms) mapped onto
  values. Legend guides for various scales are integrated
  if possible.

  Guides can be specified in each scale or in
  \code{\link{guides}}. \code{guide="legend"} in scale is
  syntax suger for \code{guide=guide_legend()}. As for how
  to specify the guide for each scales in more detail, see
  \code{\link{guides}}.
}
\examples{
# ggplot objects

p1 <- function()ggplot(melt(outer(1:4, 1:4)), aes(x = X1, y = X2)) + geom_tile(aes(fill = value))
p2 <- function()ggplot(melt(outer(1:4, 1:4)), aes(x = X1, y = X2)) + geom_tile(aes(fill = value)) + geom_point(aes(size = value))

# basic form

# short version
p1() + scale_fill_continuous(guide = "legend")

# long version
p1() + scale_fill_continuous(guide = guide_legend())

# separately set the direction of each guide

p2() + scale_fill_continuous(guide = guide_legend(direction = "horizontal")) +
scale_size(guide = guide_legend(direction = "vertical"))

# guide title

p1() + scale_fill_continuous(guide = guide_legend(title = "V")) # title text
p1() + scale_fill_continuous(name = "V") # same
p1() + scale_fill_continuous(guide = guide_legend(title = NULL)) # no title

# control styles

# key size
p1() + scale_fill_continuous(guide = guide_legend(keywidth = 3, keyheight = 1))

# title position
p1() + scale_fill_continuous(guide = guide_legend(title = "LEFT", title.position = "left"))

# title text styles
p1() + scale_fill_continuous(guide = guide_legend(title = "ROTATE", title.position = "left", title.angle = 90, title.hjust = 0.5))

# title text styles via theme_text
p1() + scale_fill_continuous(guide = guide_legend(title.theme = theme_text(size=15, face="italic", col="red", angle=45)))

# label position
p1() + scale_fill_continuous(guide = guide_legend(label.position = "bottom"))

# label styles
p1() + scale_fill_continuous(breaks=c(5, 10, 15), labels=paste("long", c(5, 10, 15)),
guide = guide_legend(direction="horizontal", title.position="top", title.theme=theme_text(hjust=0.5),
label.position="bottom", label.angle = 90, label.hjust=0.5, label.vjust=0.5))

# set aesthetic of legend key

# very low alpha value make it difficult to see legend key
p3 <- function()ggplot(melt(outer(1:4, 1:4)), aes(x = X1, y = X2)) + geom_tile(aes(fill = value), alpha = 0.1)
p3()

# set.aes overwrites the alpha
p3() + scale_fill_continuous(guide=guide_legend(set.aes = list(alpha=1)))

# combine colorbar and legend guide
p2() + scale_fill_continuous(guide = "colorbar") + scale_size(guide = "legend")

# same, but short version
p2() + guides(fill = "colorbar", size = "legend")
}
\seealso{
  \code{\link{guides}}, \code{\link{guide_colorbar}}
}

