% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/guide-.R, R/guide-axis.R,
%   R/guide-axis-logticks.R, R/guide-axis-stack.R, R/guide-axis-theta.R,
%   R/guide-legend.R, R/guide-bins.R, R/guide-colorbar.R, R/guide-colorsteps.R,
%   R/guide-custom.R, R/guide-none.R, R/guide-old.R
\docType{data}
\name{Guide}
\alias{Guide}
\alias{GuideAxis}
\alias{GuideAxisLogticks}
\alias{GuideAxisStack}
\alias{GuideAxisTheta}
\alias{GuideLegend}
\alias{GuideBins}
\alias{GuideColourbar}
\alias{GuideColoursteps}
\alias{GuideCustom}
\alias{GuideNone}
\alias{GuideOld}
\title{Guides}
\description{
The \verb{guide_*} functions (like \code{guide_legend()}) return \verb{Guide*} objects
(like \code{GuideLegend}). The \verb{Guide*} object is responsible for rendering the
guide for at least one aesthetic.
}
\details{
Each of the \verb{Guide*} objects is a \code{\link[=ggproto]{ggproto()}} object, descended from the
top-level \code{Guide}, and each implements various methods and fields.

Building a guide has three stages:
\enumerate{
\item The guide extracts the relevant information from scales.
\item The guide interacts with other parts of the plot, like coords or layers to
supplement information.
\item The guide is rendered.
}

When creating a new Guide class, you may want to consider overriding one or
more of the following:
\itemize{
\item The \code{params}, \code{elements}, \code{hashables} and \code{available_aes} fields.
\item The \code{extract_key()}, \code{extract_decor()} and \code{extract_params()} methods.
\item The \code{transform()} or \code{get_layer_key()} methods.
\item The \code{setup_params()} and \code{override_elements()} methods.
\item Any of the \verb{build_*} methods.
\item As a last resort the \code{measure_grobs()}, \code{arrange_layout()}, and
\code{assemble_drawing()} methods.
}
}
\section{Fields}{

\describe{
\item{\code{params}}{A list of initial parameters that the guide needs to
function. The base \code{Guide$params} contains mandatory parameters,
but extensions can  add new parameters. It has the following roles:
\itemize{
\item It provides the default values for parameters.
\item \code{names(params)} determines what are valid arguments for \code{new_guide()}.
\item During build stages, a mutable copy of \code{params} holds information
about the guide.
}}

\item{\code{available_aes}}{A character vector of aesthetic names for which the
guide is appropriate. Can use keyword \code{"any"} to indicate all non-position
aesthetics.}

\item{\code{elements}}{A named list of strings stating which theme elements this
guide uses. By default, strings will be translated in
\code{Guide$setup_elements()} using \code{calc_element()}. Strings are expected to
occur in \code{names(get_element_tree())}, like \code{"legend.text"} for example.
Position guides typically append the  \verb{\{aes\}.\{position\}} suffix in the
\code{setup_elements()} method when the position is known.}

\item{\code{hashables}}{A list of calls or names constructed by \code{rlang::exprs()}
indicating objects in the \code{params} field. These will be evaluated in the
context of the \code{params} field and the resulting list will be hashed. The
hash uniquely identify guides that can merge. Guides that have different
hashes will not merge. For extensions, you should include objects that
clearly mark two guides from one another that cannot be merged.}

\item{\code{train}}{\strong{Description}

A function method for orchestrating the training of a guide, which extracts
necessary information from a Scale object. As orchestrator, this method is
not intended for extension.

\strong{Usage}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Guide$train(params, scale, aesthetic, ...)
}\if{html}{\out{</div>}}

\strong{Arguments}
\describe{
\item{\code{params}}{A list of parameters initiated by the \code{params} field.}
\item{\code{scale}}{A \verb{<Scale>} ggproto object. In the case of position
guides, can be a \verb{<ViewScale>} ggproto object.}
\item{\code{aesthetic}}{A scalar string specifying the aesthetic.
If missing (default), it will use the first aesthetic specified in the
scale.}
\item{\code{...}}{Additional parameters passed on to the \code{extract_params()}
method.}
}

\strong{Value}

A modified list of parameters}

\item{\code{extract_key}}{\strong{Description}

A function method for extracting break information from the scale called
the 'key'. It retrieves breaks, maps these breaks and derives labels. These
form the basis for tick marks and labels in some guides. It is appropriate
to override in extensions.

\strong{Usage}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Guide$extract_key(scale, aesthetic, ...)
}\if{html}{\out{</div>}}

\strong{Arguments}
\describe{
\item{\code{scale}}{A \verb{<Scale>} ggproto object. In the case of position
guides, can be a \verb{<ViewScale>} ggproto object.}
\item{\code{aesthetic}}{A scalar string specifying the aesthetic.}
\item{\code{...}}{Optional arguments from the \code{params} field.}
}

\strong{Value}

A 'key' data frame containing annotated scale breaks, including at least a
column for the aesthetic, \code{.label} and \code{.value}. If there are no breaks,
returns \code{NULL}.}

\item{\code{extract_decor}}{\strong{Description}

A function method for extracting 'decor' from the scale. The 'decor' acts
as a wildcard for anything the guide may need to render that is not based
on the key. For this reason, it has guide specific meaning that indicates
different things for different guides. In \code{guide_colourbar()} it is the
colour gradient, but in \code{guide_axis()} it is the axis line information.
It is appropriate to override in extensions.

\strong{Usage}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Guide$extract_decor(scale, aesthetic, ...)
}\if{html}{\out{</div>}}

\strong{Arguments}
\describe{
\item{\code{scale}}{A \verb{<Scale>} ggproto object. In the case of position
guides, can be a \verb{<ViewScale>} ggproto object.}
\item{\code{aesthetic}}{A scalar string specifying the aesthetic.}
\item{\code{...}}{Optional arguments from the \code{params} field.}
}

\strong{Value}

Undefined. \code{NULL} by default.}

\item{\code{extract_params}}{\strong{Description}

A function method for extracting any other information from the scale that
the guide may need. A typical example is to derive the title from the scale,
or apply any edits to the \code{key} or \code{decor} variables.

\strong{Usage}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Geom$extract_params(scale, params, ...)
}\if{html}{\out{</div>}}

\strong{Arguments}
\describe{
\item{\code{scale}}{A \verb{<Scale>} ggproto object. In the case of position
guides, can be a \verb{<ViewScale>} ggproto object.}
\item{\code{params}}{A list of parameters initiated by the \code{params} field,
which at this point includes the \code{key} and \code{decor} from previous
extractors.}
\item{\code{...}}{Additional arguments passed from the \code{train()} method. For
non-position guides, often includes \code{title} as derived from the
\code{plot$labels} field.}
}

\strong{Value}

A modified list of parameters}

\item{\code{transform}}{\strong{Description}

A function method to apply coord transformation and munching to the
'key' and 'decor' parameters. This method only applies to position guides
like \code{guide_axis()} and is not called for non-position guides. It is
recommended to override this method if you have a position guide that
does not inherit from \code{GuideAxis} or has custom key' or 'decor' structures
that \code{GuideAxis$transform()} does not handle well.

\strong{Usage}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Guide$transform(params, coord, ...)
}\if{html}{\out{</div>}}

\strong{Arguments}
\describe{
\item{\code{params}}{A list of parameters initiated by the \code{params} field.}
\item{\code{coord}}{A \verb{<Coord>} ggproto object.}
\item{\code{...}}{Optional arguments, typically \code{panel_params} for most
position guides.}
}

\strong{Value}

A list of parameters. The default throws an error.}

\item{\code{merge}}{\strong{Description}

A function method for combining information from two guides. When two
guides have the same computed \code{hash} parameter derived from the \code{hashables}
field, this function will be called to merge them. If more than two guides
need to be merged, they are merged successively in a \code{Reduce()}-like
fashion.

Merging guides is the mechanism by which \code{guide_legend()} can take one
guide trained on the \code{shape} scale, another trained on the \code{colour} scale
and display them together in the same guide, for example.

Overriding this method is recommended if the extension descends directly
from \code{Guide} and not its children. Otherwise, it should be overridden if
presented with no superior options.

\strong{Usage}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Guide$merge(params, new_guide, new_params)
}\if{html}{\out{</div>}}

\strong{Arguments}
\describe{
\item{\code{params}}{A list of parameters derived from the \code{params} field of
this guide.}
\item{\code{new_guide}}{A \verb{<Guide>} ggproto object representing the other guide class}
\item{\code{new_params}}{A list of parameters derived from the \code{params} field
of the other guide}
}

\strong{Value}

A named list containing \code{guide} and \code{params}, where \code{guide} is a \verb{<Guide>}
ggproto object and \code{params} is a list with parameters. By default, returns
the new guide and its parameters.}

\item{\code{process_layers,get_layer_key}}{\strong{Description}

These function methods extract information from layers that the guide may
need. The \code{process_layers()} method is tasked with selecting layers that
are represented by the guide and are to be included. The selected layers
should be passed on to the \code{get_layer_key()} method.

Typical use of these methods is for \code{guide_legend()} to extract the
\code{Geom$draw_key} function to render glyphs in addition to any default or
fixed aesthetics. While these methods are called in position guides,
the \code{layers} and \code{data} arguments are empty as these are unavailable at
that point.

You can override \code{get_layer_key()}, but \code{process_layers()} should
probably only be overridden if the extension does not inherit from
\code{GuideLegend}.

\strong{Usage}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Guide$process_layers(params, layers, data, theme)
Guide$get_layer_key(params, layers, data, theme)
}\if{html}{\out{</div>}}

\strong{Arguments}
\describe{
\item{\code{params}}{A list of parameters initiated by the \code{params} field.}
\item{\code{layers}}{A list of layers from \code{plot$layers}.}
\item{\code{data}}{A list of layer data frames.}
\item{\code{theme}}{A \link[=complete_theme]{completed theme} object.}
}

\strong{Value}

A list of parameters}

\item{\code{draw}}{\strong{Description}

A function method is the main orchestrator for drawing the guide. It sets
up the final pieces in context of the position, direction and theme
and. Subsequenty, it renders the individual components like titles, ticks,
labels and decor. Finally, it arranges these components into a guide.

This method should only be overridden if the extension has non standard
components that do not fit into 'decor' or when this method can be greatly
simplified for humble guides. All subsidiaries are fine to override.

\strong{Usage}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Geom$setup_params(theme, position, direction, params)
}\if{html}{\out{</div>}}

\strong{Arguments}
\describe{
\item{\code{theme}}{A \link[=complete_theme]{complete theme} object.}
\item{\code{position}}{A scalar string indicating the position where
the guide should be drawn. Typically \code{"top"}, \code{"right"}, \code{"bottom"}
or \code{"left"}, unless it is a position guide for an exotic coord. Can be
\code{NULL}, in which case \code{params$position} should be used.}
\item{\code{direction}}{A scalar string indicating the legend direction.
Can be \code{NULL}, in which case \code{params$direction} should be used.}
\item{\code{params}}{A list of parameters initiated by the \code{params} field.}
}
\strong{Value}

A grob with the guide.}

\item{\code{draw_early_exit}}{\strong{Description}

A function method that determines what should be drawn when the guide 'key'
is empty. The default method returns \code{\link[=zeroGrob]{zeroGrob()}}. You can override
this method if an empty key should draw anything. Used in \code{guide_axis()}
to render the \code{axis.line} part even if no ticks or labels should be drawn.

\strong{Usage}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Guide$draw_early_exit(params, elements)
}\if{html}{\out{</div>}}

\strong{Arguments}
\describe{
\item{\code{params}}{A list of parameters initiated by the \code{params} field.}
\item{\code{elements}}{A list of elements or resolved theme settings from
the \code{override_elements()} method.}
}

\strong{Value}

A grob.}

\item{\code{setup_params}}{\strong{Description}

A function method for finalising parameters. Typically used to make checks
on the \code{params} object or to make any position or direction based
adjustments.

\strong{Usage}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Guide$setup_params(params)
}\if{html}{\out{</div>}}

\strong{Arguments}
\describe{
\item{\code{params}}{A list of parameters initiated by the \code{params} field.}
}

\strong{Value}

A list of parameters}

\item{\code{setup_elements,override_elements}}{\strong{Description}

A function method for resolving required theme elements. The
\code{setup_elements()} method joins local guide theme with global theme and
calculates the necessary theme elements. The \code{override_elements()} method
is a hook to edit elements after they've been calculated.

You can override the \code{setup_elements()} method if you need more complicated
theme handling before calculating elements or want to intervene in inheritance.
For example, \code{guide_legend()} has special handling of text margins and
\code{guide_axis()} suffixes \verb{\{aes\}.\{position\}} to get the theme elements for
the correct position.

For other purposes, you can override the \code{override_elements()} method.

\strong{Usage}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Guide$setup_elements(params, elements, theme)
Guide$override_elements(params, elements, theme)
}\if{html}{\out{</div>}}

\strong{Arguments}
\describe{
\item{\code{params}}{A list of parameters initiated by the \code{params} field.}
\item{\code{elements}}{A named list of strings initiated by the \code{elements} field.}
\item{\code{theme}}{A \link[=complete_theme]{complete theme}}
}

\strong{Value}

A list of elements or resolved theme settings.}

\item{\code{build_title}}{\strong{Description}

A function method for rendering the title. Note that titles for position
guides are rendered by the Facet class and not this method.

You can override this method if you need to render more than one title
(or none) or adjust margin settings.

\strong{Usage}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Guide$build_title(label, elements, params)
}\if{html}{\out{</div>}}

\strong{Arguments}
\describe{
\item{\code{label}}{A single string or expression with the title text.}
\item{\code{elements}}{A list of elements or resolved theme settings from
the \code{override_elements()} method. The default method expects
\code{elements$title} to inherit from the \verb{<element_text>} class.}
\item{\code{params}}{A list of parameters initiated by the \code{params} field.}
}

\strong{Value}

A grob representing the title.}

\item{\code{build_ticks}}{\strong{Description}

A function method for rendering tick marks.

You can override this function if you don't need ticks or have completely
different logic on how these should be drawn.

\strong{Usage}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Guide$build_ticks(key, elements, params, position, length)
}\if{html}{\out{</div>}}

\strong{Arguments}
\describe{
\item{\code{key}}{A data frame with the key information derived from the
\code{extract_key()} method.}
\item{\code{elements}}{A list of elements or resolved theme settings from
the \code{override_elements()} method. The default method expects
\code{elements$ticks} to inherit from the \verb{<element_line>} class and
\code{elements$ticks_length} to be a scalar \verb{<unit>} object.}
\item{\code{params}}{A list of parameters initiated by the \code{params} field.}
\item{\code{position}}{A scalar string indicating the position. Due to
historic error this works in the opposite way to intuition: if you want
ticks for an axis at the bottom of a plot, you should use \code{"top"} here.}
\item{\code{length}}{A scalar \verb{<unit>} object giving the tick length.}
}

\strong{Value}

A grob representing tick marks.}

\item{\code{build_labels}}{\strong{Description}

A function method for rendering labels. The default method returns an
empty grob. It is recommended to override this method when your extension
directly descends from Guide.

\strong{Usage}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Guide$build_labels(key, elements, params)
}\if{html}{\out{</div>}}

\strong{Arguments}
\describe{
\item{\code{key}}{A data frame with the key information derived from the
\code{extract_key()} method.}
\item{\code{elements}}{A list of elements or resolved theme settings from
the \code{override_elements()} method. Most non-default methods expects
\code{elements$text} to inherit from the \verb{<element_text>}.}
\item{\code{params}}{A list of parameters initiated by the \code{params} field.}
}

\strong{Value}

A grob representing labels.}

\item{\code{build_decor}}{\strong{Description}

A function method for rendering decor. As the 'wildcard' component, this
can draw whatever component the guide needs that isn't already captured by
the key. The default method returns an empty grob. It is recommended to
override this method.

For some examples: \code{guide_legend()} renders the keys with the glyphs,
\code{guide_colourbar()} renders the colour gradient rectangle and
\code{guide_axis()} renders the axis line.

\strong{Usage}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Guide$build_decor(decor, grobs, elements, params)
}\if{html}{\out{</div>}}

\strong{Arguments}
\describe{
\item{\code{decor}}{A data frame (or other structure) with information derived
from the \code{extract_decor()} method.}
\item{\code{grobs}}{A list with grobs generated by the other \verb{build_*}
methods.}
\item{\code{elements}}{A list of elements or resolved theme settings from
the \code{override_elements()} method. Most non-default methods expects
\code{elements$text} to inherit from the \verb{<element_text>}.}
\item{\code{params}}{A list of parameters initiated by the \code{params} field.}
}

\strong{Value}

A grob.}

\item{\code{measure_grobs}}{\strong{Description}

A function method for measuring grobs. In preparation for arranging grobs,
they often need to be measured to determine their widths and heights.
It is convention that every measurement is converted to centimetres.
You can override this method if your extension directly descends from
Guide, or the parent class measurement is defective.

\strong{Usage}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Guide$measure_grobs(grobs, params, elements)
}\if{html}{\out{</div>}}

\strong{Arguments}
\describe{
\item{\code{grobs}}{A list with grobs generated by the \verb{build_*} methods.}
\item{\code{params}}{A list of parameters initiated by the \code{params} field.}
\item{\code{elements}}{A list of elements or resolved theme settings from
the \code{override_elements()} method.}
}

\strong{Value}

A named list or \verb{<unit>} vector giving sizes of components, coordinated
with \code{arrange_layout()} and \code{assemble_drawing()} methods. The default
method returns \code{NULL}.}

\item{\code{arrange_layout}}{\strong{Description}

A function method for determining the location or order of grobs in a
gtable. Typically determines rows and columns where decor and labels are
placed. Titles are added seperately.You can override this method if your
extension directly descends from Guide.

\strong{Usage}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Guide$arrange_layout(key, sizes, params, elements)
}\if{html}{\out{</div>}}

\strong{Arguments}
\describe{
\item{\code{key}}{A data frame with the key information derived from the
\code{extract_key()} method.}
\item{\code{sizes}}{A list of \verb{<unit>} vector from the \code{measure_grobs()} method.}
\item{\code{params}}{A list of parameters initiated by the \code{params} field.}
\item{\code{elements}}{A list of elements or resolved theme settings from
the \code{override_elements()} method.}
}

\strong{Value}

Any structure holding placement information coordinated with the
\code{assemble_drawing()} method.}

\item{\code{assemble_drawing}}{\strong{Description}

A function method that takes measurements, placement information and grobs
and assembles these together in a gtable structure. You can override this
method if your extension directly descends from Guide, or the parent class
assembly does not work for your guide.

\strong{Usage}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Guide$assemble_drawing(grobs, layout, sizes, params, elements)
}\if{html}{\out{</div>}}

\strong{Arguments}
\describe{
\item{\code{grobs}}{A list with grobs generated by the \verb{build_*} methods.}
\item{\code{layout}}{A data structure from the \code{arrange_layout()} method.}
\item{\code{sizes}}{A list of \verb{<unit>} vector from the \code{measure_grobs()} method.}
\item{\code{params}}{A list of parameters initiated by the \code{params} field.}
\item{\code{elements}}{A list of elements or resolved theme settings from
the \code{override_elements()} method.}
}

\strong{Value}

A finished gtable containing the guide.}

\item{\code{arrange_layout}}{\strong{Description}

A function method for placing the title. It is a subsidiary method used
in the \code{assemble_drawing()} method for non-position guides. Titles are
typically added before \code{legend.margin} is applied. It is not
recommended to override this method.

\strong{Usage}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Guide$add_title(gtable, title, position, just)
}\if{html}{\out{</div>}}

\strong{Arguments}
\describe{
\item{\code{gtable}}{An unfinished gtable under construction in the
\code{assemble_drawing()} method.}
\item{\code{title}}{The grob resulting from the \code{build_title()} method.}
\item{\code{position}}{A scaler string, either \code{"top"}, \code{"right"}, \code{"bottom"}
or \code{"left"} corresponding to the \code{legend.title.position}.}
\item{\code{just}}{A named list having \code{hjust} and \code{vjust} components with
scalar numeric values between 0 and 1.}
}

\strong{Value}

The \code{gtable} argument with added title.}
}}

\section{Conventions}{


The object name that a new class is assigned to is typically the same as the
class name. Guide class names are in UpperCamelCase and start with the
\verb{Guide*} prefix, like \code{GuideNew}.

A constructor function is usually paired with a Guide class. The constructor
wraps a call to \code{new_guide()}, where e.g. \code{new_guide(super = GuideNew)}. The
constructor name is formatted by taking the Guide class name and formatting
it with snake_case, so that \code{GuideNew} becomes \code{guide_new()}.
}

\examples{
# Extending the class
GuideDescribe <- ggproto(
  "GuideDescribe", Guide,
  # Fields
  elements  = list(text = "legend.text", margin = "legend.margin"),
  hashables = rlang::exprs(key$.label),

  # Methods
  build_title = function(...) zeroGrob(), # Turn off title

  build_labels = function(key, elements, params) {
    labels <- key$.label
    n <- length(labels)
    labels <- paste0(paste0(labels[-n], collapse = ", "), ", and ", labels[n])
    labels <- paste0("A guide showing ", labels, " categories")
    element_grob(elements$text, label = labels, margin_x = TRUE, margin_y = TRUE)
  },

  measure_grobs = function(grobs, params, elements) {
    # Measuring in centimetres is the convention
    width  <- grid::convertWidth(grid::grobWidth(grobs$labels), "cm", valueOnly = TRUE)
    height <- grid::convertHeight(grid::grobHeight(grobs$labels), "cm", valueOnly = TRUE)
    list(width = unit(width, "cm"), height = unit(height, "cm"))
  },

  assemble_drawing = function(self, grobs, layout, sizes, params, elements) {
    gt <- gtable::as.gtable(grobs$labels, width = sizes$width, height = sizes$height)
    gt <- gtable::gtable_add_padding(gt, elements$margin)
    gt
  }
)

# Building a constructor
guide_describe <- function(position = NULL) {
  new_guide(position = position, super = GuideDescribe)
}

# Use new guide plot
ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point() +
  guides(colour = guide_describe("bottom"))
}
\seealso{
Run \code{vignette("extending-ggplot2")}, in particular the "Creating
new guides" section.
}
\keyword{datasets}
\keyword{internal}
