\name{guide_colorbar}
\alias{guide_colorbar}
\title{Colorbar guide}
\usage{
  guide_colorbar(title = waiver(), title.position = NULL,
    title.theme = NULL, title.hjust = NULL,
    title.vjust = NULL, label = TRUE,
    label.position = NULL, label.theme = NULL,
    label.hjust = NULL, label.vjust = NULL,
    barwidth = NULL, barheight = NULL, nbin = 20,
    raster = TRUE, ticks = TRUE, draw.ulim = TRUE,
    draw.llim = TRUE, direction = NULL,
    default.unit = "line", ...)
}
\arguments{
  \item{title}{A character string or expression indicating
  a title of guide. If \code{NULL}, the title is not shown.
  By default (\code{\link{waiver}}), the name of the scale
  object or tha name specified in \code{\link{labs}} is
  used for the title.}

  \item{title.position}{A character string indicating the
  position of a title. One of "top" (default for a vertical
  guide), "bottom", "left" (default for a horizontal
  guide), or "right."}

  \item{title.theme}{A theme object for rendering the title
  text. Usually the object of \code{\link{theme_text}} is
  expected. By default, the theme is specified by
  \code{legend.title} in \code{\link{opts}} or theme.}

  \item{title.hjust}{A numeric specifying horizontal
  justification of the title text.}

  \item{title.vjust}{A numeric specifying vertical
  justification of the title text.}

  \item{label}{logical. If \code{TRUE} then the labels are
  drawn. If \code{FALSE} then the labels are invisible.}

  \item{label.position}{A character string indicating the
  position of a label. One of "top", "bottom" (default for
  horizontal guide), "left", or "right" (default for
  vertical gudie).}

  \item{label.theme}{A theme object for rendering the label
  text. Usually the object of \code{\link{theme_text}} is
  expected. By default, the theme is specified by
  \code{legend.text} in \code{\link{opts}} or theme.}

  \item{label.hjust}{A numeric specifying horizontal
  justification of the label text.}

  \item{label.vjust}{A numeric specifying vertical
  justification of the label text.}

  \item{barwidth}{A numeric or a unit object specifying the
  width of the colorbar. Default value is
  \code{legend.key.width} or \code{legend.key.size} in
  \code{\link{opts}} or theme.}

  \item{barheight}{A numeric or a unit object specifying
  the height of the colorbar. Default value is
  \code{legend.key.height} or \code{legend.key.size} in
  \code{\link{opts}} or theme.}

  \item{nbin}{A numeric specifying the number of bins for
  drawing colorbar. A smoother colorbar for a larger
  value.}

  \item{raster}{A logical. If \code{TRUE} then the colorbar
  is rendered as a raster object. If \code{FALSE} then the
  colorbar is rendered as a set of rectangles. Note that
  not all graphics devices are capable of rendering raster
  image.}

  \item{ticks}{A logical specifying if tick marks on
  colorbar should be visible.}

  \item{draw.ulim}{A logical specifying if the upper limit
  tick marks should be visible.}

  \item{draw.llim}{A logical specifying if the lower limit
  tick marks should be visible.}

  \item{direction}{A character string indicating the
  direction of the guide. One of "horizontal" or
  "vertical."}

  \item{default.unit}{A character string indicating unit
  for \code{barwidth} and \code{barheight}.}

  \item{...}{ignored.}
}
\value{
  Guide object
}
\description{
  Colorbar type guide
}
\details{
  Colorbar type guide shows a continuous color scales
  mapped onto values. Colorbar is available with
  \code{scale_fill} and \code{scale_colour}. For more
  information, see
  \href{http://www.mathworks.com/help/techdoc/ref/colorbar.html}{Matlab
  site}.

  Guides can be specified in each scale or in
  \code{\link{guides}}. \code{guide="legend"} in scale is
  syntax suger for \code{guide=guide_legend()}. As for how
  to specify the guide for each scales, see
  \code{\link{guides}}.
}
\examples{
# ggplot objects

p1 <- function()ggplot(melt(outer(1:4, 1:4), varnames = c("X1", "X2")), aes(x = X1, y = X2)) + geom_tile(aes(fill = value))
p2 <- function()ggplot(melt(outer(1:4, 1:4), varnames = c("X1", "X2")), aes(x = X1, y = X2)) + geom_tile(aes(fill = value)) + geom_point(aes(size = value))


## basic form

# short version
p1() + scale_fill_continuous(guide = "colorbar")

# long version

p1() + scale_fill_continuous(guide = guide_colorbar())

# separately set the direction of each guide
p2() + scale_fill_continuous(guide = guide_colorbar(direction = "horizontal")) +
  scale_size(guide = guide_legend(direction = "vertical")) ## separately set the direction of each gui

## control styles

# bar size
p1() + scale_fill_continuous(guide = guide_colorbar(barwidth=0.5, barheight=10))

# no label
p1() + scale_fill_continuous(guide = guide_colorbar(label = FALSE))

# no tick marks
p1() + scale_fill_continuous(guide = guide_colorbar(ticks = FALSE))

# label position
p1() + scale_fill_continuous(guide = guide_colorbar(label.position = "left"))

# label theme
p1() + scale_fill_continuous(guide = guide_colorbar(label.theme = theme_text(col="blue")))

# small number of bins
p1() + scale_fill_continuous(guide = guide_colorbar(nbin = 3))

# large number of bins
p1() + scale_fill_continuous(guide = guide_colorbar(nbin = 100))

# make top- and bottom-most ticks invisible
p1() + scale_fill_continuous(limits=c(0,20), breaks=c(0,5,10,15,20),
                            guide = guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE))

# combine colorbar and legend guide
p2() + scale_fill_continuous(guide = "colorbar") + scale_size(guide = "legend")

# same, but short version
p2() + guides(fill = "colorbar", size = "legend")

# non-raster colorbar
p1() + scale_fill_continuous(guide = guide_colorbar(raster = FALSE))
}
\seealso{
  \code{\link{guides}}, \code{\link{guide_legend}}
}

