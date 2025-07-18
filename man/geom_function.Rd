% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom-function.R, R/stat-function.R
\name{geom_function}
\alias{geom_function}
\alias{stat_function}
\title{Draw a function as a continuous curve}
\usage{
geom_function(
  mapping = NULL,
  data = NULL,
  stat = "function",
  position = "identity",
  ...,
  arrow = NULL,
  arrow.fill = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_function(
  mapping = NULL,
  data = NULL,
  geom = "function",
  position = "identity",
  ...,
  fun,
  xlim = NULL,
  n = 101,
  args = list(),
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

\item{data}{Ignored by \code{stat_function()}, do not use.}

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

\item{arrow}{Arrow specification, as created by \code{\link[grid:arrow]{grid::arrow()}}.}

\item{arrow.fill}{fill colour to use for the arrow head (if closed). \code{NULL}
means use \code{colour} aesthetic.}

\item{lineend}{Line end style (round, butt, square).}

\item{linejoin}{Line join style (round, mitre, bevel).}

\item{linemitre}{Line mitre limit (number greater than 1).}

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

\item{fun}{Function to use. Either 1) an anonymous function in the base or
rlang formula syntax (see \code{\link[rlang:as_function]{rlang::as_function()}})
or 2) a quoted or character name referencing a function; see examples. Must
be vectorised.}

\item{xlim}{Optionally, specify the range of the function.}

\item{n}{Number of points to interpolate along the x axis.}

\item{args}{List of additional arguments passed on to the function defined by \code{fun}.}
}
\description{
Computes and draws a function as a continuous curve. This makes it easy to
superimpose a function on top of an existing plot. The function is called
with a grid of evenly spaced values along the x axis, and the results are
drawn (by default) with a line.
}
\section{Computed variables}{

These are calculated by the 'stat' part of layers and can be accessed with \link[=aes_eval]{delayed evaluation}.
\itemize{
\item \code{after_stat(x)}\cr \code{x} values along a grid.
\item \code{after_stat(y)}\cr values of the function evaluated at corresponding \code{x}.
}
}

\examples{

# geom_function() is useful for overlaying functions
set.seed(1492)
ggplot(data.frame(x = rnorm(100)), aes(x)) +
  geom_density() +
  geom_function(fun = dnorm, colour = "red")

# To plot functions without data, specify range of x-axis
base <-
  ggplot() +
  xlim(-5, 5)

base + geom_function(fun = dnorm)

base + geom_function(fun = dnorm, args = list(mean = 2, sd = .5))

# The underlying mechanics evaluate the function at discrete points
# and connect the points with lines
base + stat_function(fun = dnorm, geom = "point")

base + stat_function(fun = dnorm, geom = "point", n = 20)

base + stat_function(fun = dnorm, geom = "polygon", color = "blue", fill = "blue", alpha = 0.5)

base + geom_function(fun = dnorm, n = 20)

# Two functions on the same plot
base +
  geom_function(aes(colour = "normal"), fun = dnorm) +
  geom_function(aes(colour = "t, df = 1"), fun = dt, args = list(df = 1))

# Using a custom anonymous function
base + geom_function(fun = \(x) 0.5 * exp(-abs(x)))
# or using lambda syntax:
# base + geom_function(fun = ~ 0.5 * exp(-abs(.x)))
# or using a custom named function:
# f <- function(x) 0.5 * exp(-abs(x))
# base + geom_function(fun = f)

# Using xlim to restrict the range of function
ggplot(data.frame(x = rnorm(100)), aes(x)) +
geom_density() +
geom_function(fun = dnorm, colour = "red", xlim=c(-1, 1))

# Using xlim to widen the range of function
ggplot(data.frame(x = rnorm(100)), aes(x)) +
geom_density() +
geom_function(fun = dnorm, colour = "red", xlim=c(-7, 7))

}
\seealso{
\code{\link[rlang:as_function]{rlang::as_function()}}
}
\section{Aesthetics}{

\code{geom_function()} understands the following aesthetics. Required aesthetics are displayed in bold and defaults are displayed for optional aesthetics:
\tabular{rll}{
 • \tab \strong{\code{\link[=aes_position]{x}}} \tab   \cr
 • \tab \strong{\code{\link[=aes_position]{y}}} \tab   \cr
 • \tab \code{\link[=aes_colour_fill_alpha]{alpha}} \tab → \code{NA} \cr
 • \tab \code{\link[=aes_colour_fill_alpha]{colour}} \tab → via \code{theme()} \cr
 • \tab \code{\link[=aes_group_order]{group}} \tab → inferred \cr
 • \tab \code{\link[=aes_linetype_size_shape]{linetype}} \tab → via \code{theme()} \cr
 • \tab \code{\link[=aes_linetype_size_shape]{linewidth}} \tab → via \code{theme()} \cr
}

Learn more about setting these aesthetics in \code{vignette("ggplot2-specs")}.
}
