% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom-ribbon.R, R/stat-align.R
\name{geom_ribbon}
\alias{geom_ribbon}
\alias{geom_area}
\alias{stat_align}
\title{Ribbons and area plots}
\usage{
geom_ribbon(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  orientation = NA,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  outline.type = "both",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_area(
  mapping = NULL,
  data = NULL,
  stat = "align",
  position = "stack",
  ...,
  orientation = NA,
  outline.type = "upper",
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_align(
  mapping = NULL,
  data = NULL,
  geom = "area",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[=aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[=ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[=fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this layer.
When using a \verb{geom_*()} function to construct a layer, the \code{stat}
argument can be used to override the default coupling between geoms and
stats. The \code{stat} argument accepts the following:
\itemize{
\item A \code{Stat} ggproto subclass, for example \code{StatCount}.
\item A string naming the stat. To give the stat as a string, strip the
function name of the \code{stat_} prefix. For example, to use \code{stat_count()},
give the stat as \code{"count"}.
\item For more information and other ways to specify the stat, see the
\link[=layer_stats]{layer stat} documentation.
}}

\item{position}{A position adjustment to use on the data for this layer. This
can be used in various ways, including to prevent overplotting and
improving the display. The \code{position} argument accepts the following:
\itemize{
\item The result of calling a position function, such as \code{position_jitter()}.
This method allows for passing extra arguments to the position.
\item A string naming the position adjustment. To give the position as a
string, strip the function name of the \code{position_} prefix. For example,
to use \code{position_jitter()}, give the position as \code{"jitter"}.
\item For more information and other ways to specify the position, see the
\link[=layer_positions]{layer position} documentation.
}}

\item{...}{Other arguments passed on to \code{\link[=layer]{layer()}}'s \code{params} argument. These
arguments broadly fall into one of 4 categories below. Notably, further
arguments to the \code{position} argument, or aesthetics that are required
can \emph{not} be passed through \code{...}. Unknown arguments that are not part
of the 4 categories below are ignored.
\itemize{
\item Static aesthetics that are not mapped to a scale, but are at a fixed
value and apply to the layer as a whole. For example, \code{colour = "red"}
or \code{linewidth = 3}. The geom's documentation has an \strong{Aesthetics}
section that lists the available options. The 'required' aesthetics
cannot be passed on to the \code{params}. Please note that while passing
unmapped aesthetics as vectors is technically possible, the order and
required length is not guaranteed to be parallel to the input data.
\item When constructing a layer using
a \verb{stat_*()} function, the \code{...} argument can be used to pass on
parameters to the \code{geom} part of the layer. An example of this is
\code{stat_density(geom = "area", outline.type = "both")}. The geom's
documentation lists which parameters it can accept.
\item Inversely, when constructing a layer using a
\verb{geom_*()} function, the \code{...} argument can be used to pass on parameters
to the \code{stat} part of the layer. An example of this is
\code{geom_area(stat = "density", adjust = 0.5)}. The stat's documentation
lists which parameters it can accept.
\item The \code{key_glyph} argument of \code{\link[=layer]{layer()}} may also be passed on through
\code{...}. This can be one of the functions described as
\link[=draw_key]{key glyphs}, to change the display of the layer in the legend.
}}

\item{orientation}{The orientation of the layer. The default (\code{NA})
automatically determines the orientation from the aesthetic mapping. In the
rare event that this fails it can be given explicitly by setting \code{orientation}
to either \code{"x"} or \code{"y"}. See the \emph{Orientation} section for more detail.}

\item{lineend}{Line end style (round, butt, square).}

\item{linejoin}{Line join style (round, mitre, bevel).}

\item{linemitre}{Line mitre limit (number greater than 1).}

\item{outline.type}{Type of the outline of the area; \code{"both"} draws both the
upper and lower lines, \code{"upper"}/\code{"lower"} draws the respective lines only.
\code{"full"} draws a closed polygon around the area.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display. To include legend keys for all levels, even
when no data exists, use \code{TRUE}.  If \code{NA}, all levels are shown in legend,
but unobserved levels are omitted.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[=annotation_borders]{annotation_borders()}}.}

\item{geom}{The geometric object to use to display the data for this layer.
When using a \verb{stat_*()} function to construct a layer, the \code{geom} argument
can be used to override the default coupling between stats and geoms. The
\code{geom} argument accepts the following:
\itemize{
\item A \code{Geom} ggproto subclass, for example \code{GeomPoint}.
\item A string naming the geom. To give the geom as a string, strip the
function name of the \code{geom_} prefix. For example, to use \code{geom_point()},
give the geom as \code{"point"}.
\item For more information and other ways to specify the geom, see the
\link[=layer_geoms]{layer geom} documentation.
}}
}
\description{
For each x value, \code{geom_ribbon()} displays a y interval defined
by \code{ymin} and \code{ymax}. \code{geom_area()} is a special case of
\code{geom_ribbon()}, where the \code{ymin} is fixed to 0 and \code{y} is used instead
of \code{ymax}.
}
\details{
An area plot is the continuous analogue of a stacked bar chart (see
\code{\link[=geom_bar]{geom_bar()}}), and can be used to show how composition of the
whole varies over the range of x. Choosing the order in which different
components is stacked is very important, as it becomes increasing hard to
see the individual pattern as you move up the stack. See
\code{\link[=position_stack]{position_stack()}} for the details of stacking algorithm. To facilitate
stacking, the default \code{stat = "align"} interpolates groups to a common set
of x-coordinates. To turn off this interpolation, \code{stat = "identity"} can
be used instead.
}
\section{Orientation}{

This geom treats each axis differently and, thus, can thus have two orientations. Often the orientation is easy to deduce from a combination of the given mappings and the types of positional scales in use. Thus, ggplot2 will by default try to guess which orientation the layer should have. Under rare circumstances, the orientation is ambiguous and guessing may fail. In that case the orientation can be specified directly using the \code{orientation} parameter, which can be either \code{"x"} or \code{"y"}. The value gives the axis that the geom should run along, \code{"x"} being the default orientation you would expect for the geom.
}

\examples{
# Generate data
huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
h <- ggplot(huron, aes(year))

h + geom_ribbon(aes(ymin=0, ymax=level))
h + geom_area(aes(y = level))

# Orientation cannot be deduced by mapping, so must be given explicitly for
# flipped orientation
h + geom_area(aes(x = level, y = year), orientation = "y")

# Add aesthetic mappings
h +
  geom_ribbon(aes(ymin = level - 1, ymax = level + 1), fill = "grey70") +
  geom_line(aes(y = level))

# The underlying stat_align() takes care of unaligned data points
df <- data.frame(
  g = c("a", "a", "a", "b", "b", "b"),
  x = c(1, 3, 5, 2, 4, 6),
  y = c(2, 5, 1, 3, 6, 7)
)
a <- ggplot(df, aes(x, y, fill = g)) +
  geom_area()

# Two groups have points on different X values.
a + geom_point(size = 8) + facet_grid(g ~ .)

# stat_align() interpolates and aligns the value so that the areas can stack
# properly.
a + geom_point(stat = "align", position = "stack", size = 8)

# To turn off the alignment, the stat can be set to "identity"
ggplot(df, aes(x, y, fill = g)) +
  geom_area(stat = "identity")

}
\seealso{
\code{\link[=geom_bar]{geom_bar()}} for discrete intervals (bars),
\code{\link[=geom_linerange]{geom_linerange()}} for discrete intervals (lines),
\code{\link[=geom_polygon]{geom_polygon()}} for general polygons
}
\section{Aesthetics}{

\code{geom_ribbon()} understands the following aesthetics. Required aesthetics are displayed in bold and defaults are displayed for optional aesthetics:
\tabular{rll}{
 • \tab \strong{\code{\link[=aes_position]{x}} \emph{or} \code{\link[=aes_position]{y}}} \tab   \cr
 • \tab \strong{\code{\link[=aes_position]{ymin}} \emph{or} \code{\link[=aes_position]{xmin}}} \tab   \cr
 • \tab \strong{\code{\link[=aes_position]{ymax}} \emph{or} \code{\link[=aes_position]{xmax}}} \tab   \cr
 • \tab \code{\link[=aes_colour_fill_alpha]{alpha}} \tab → \code{NA} \cr
 • \tab \code{\link[=aes_colour_fill_alpha]{colour}} \tab → via \code{theme()} \cr
 • \tab \code{\link[=aes_colour_fill_alpha]{fill}} \tab → via \code{theme()} \cr
 • \tab \code{\link[=aes_group_order]{group}} \tab → inferred \cr
 • \tab \code{\link[=aes_linetype_size_shape]{linetype}} \tab → via \code{theme()} \cr
 • \tab \code{\link[=aes_linetype_size_shape]{linewidth}} \tab → via \code{theme()} \cr
}

Learn more about setting these aesthetics in \code{vignette("ggplot2-specs")}.
}
