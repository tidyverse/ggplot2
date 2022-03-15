#' Update axis/legend labels
#'
#' @param p plot to modify
#' @param labels named list of new labels
#' @keywords internal
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' update_labels(p, list(x = "New x"))
#' update_labels(p, list(x = expression(x / y ^ 2)))
#' update_labels(p, list(x = "New x", y = "New Y"))
#' update_labels(p, list(colour = "Fail silently"))
update_labels <- function(p, labels) {
  p <- plot_clone(p)
  p$labels <- defaults(labels, p$labels)
  p
}

#' Modify axis, legend, and plot labels
#'
#' Good labels are critical for making your plots accessible to a wider
#' audience. Always ensure the axis and legend labels display the full
#' variable name. Use the plot `title` and `subtitle` to explain the
#' main findings. It's common to use the `caption` to provide information
#' about the data source. `tag` can be used for adding identification tags
#' to differentiate between multiple plots.
#'
#' You can also set axis and legend labels in the individual scales (using
#' the first argument, the `name`). If you're changing other scale options, this
#' is recommended.
#'
#' If a plot already has a title, subtitle, caption, etc., and you want to
#' remove it, you can do so by setting the respective argument to `NULL`. For
#' example, if plot `p` has a subtitle, then `p + labs(subtitle = NULL)` will
#' remove the subtitle from the plot.
#'
#' @param label The title of the respective axis (for `xlab()` or `ylab()`) or
#'        of the plot (for `ggtitle()`).
#' @param title The text for the title.
#' @param subtitle The text for the subtitle for the plot which will be
#'        displayed below the title.
#' @param caption The text for the caption which will be displayed in the
#'        bottom-right of the plot by default.
#' @param tag The text for the tag label which will be displayed at the
#'        top-left of the plot by default.
#' @param alt,alt_insight Text used for the generation of alt-text for the plot.
#'        See [get_alt_text] for examples.
#' @param ... A list of new name-value pairs. The name should be an aesthetic.
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()
#' p + labs(colour = "Cylinders")
#' p + labs(x = "New x label")
#'
#' # The plot title appears at the top-left, with the subtitle
#' # display in smaller text underneath it
#' p + labs(title = "New plot title")
#' p + labs(title = "New plot title", subtitle = "A subtitle")
#'
#' # The caption appears in the bottom-right, and is often used for
#' # sources, notes or copyright
#' p + labs(caption = "(based on data from ...)")
#'
#' # The plot tag appears at the top-left, and is typically used
#' # for labelling a subplot with a letter.
#' p + labs(title = "title", tag = "A")
#'
#' # If you want to remove a label, set it to NULL.
#' p +
#'  labs(title = "title") +
#'  labs(title = NULL)
labs <- function(..., title = waiver(), subtitle = waiver(), caption = waiver(),
                 tag = waiver(), alt = waiver(), alt_insight = waiver()) {
  # .ignore_empty = "all" is needed to allow trailing commas, which is NOT a trailing comma for dots_list() as it's in ...
  args <- dots_list(..., title = title, subtitle = subtitle, caption = caption,
    tag = tag, alt = alt, alt_insight = alt_insight, .ignore_empty = "all")

  is_waive <- vapply(args, is.waive, logical(1))
  args <- args[!is_waive]
  # remove duplicated arguments
  args <- args[!duplicated(names(args))]
  args <- rename_aes(args)

  structure(args, class = "labels")
}

#' @rdname labs
#' @export
xlab <- function(label) {
  labs(x = label)
}

#' @rdname labs
#' @export
ylab <- function(label) {
  labs(y = label)
}

#' @rdname labs
#' @export
ggtitle <- function(label, subtitle = waiver()) {
  labs(title = label, subtitle = subtitle)
}

#' Extract alt text from a plot
#'
#' This function returns a text that can be used as alt-text in webpages etc.
#' Currently it will use the `alt` label, added with `+ labs(alt = <...>)`, or
#' a return an empty string, but in the future it might try to generate an alt
#' text from the information stored in the plot.
#'
#' @param p a ggplot object
#' @param ... Currently ignored
#'
#' @return A text string
#'
#' @export
#' @aliases alt_text
#'
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#'
#' # Returns an empty string
#' get_alt_text(p)
#'
#' # A user provided alt text
#' p <- p + labs(
#'   alt = paste("A scatterplot showing the negative correlation between engine",
#'               "displacement as a function of highway miles per gallon")
#' )
#'
#' get_alt_text(p)
#'
get_alt_text <- function(p, ...) {
  UseMethod("get_alt_text")
}
#' @export
get_alt_text.ggplot <- function(p, ...) {
  p$labels[["alt"]] %||% ""
}
#' @export
get_alt_text.ggplot_built <- function(p, ...) {
  p$plot$labels[["alt"]] %||% ""
}
#' @export
get_alt_text.gtable <- function(p, ...) {
  attr(p, "alt-label") %||% ""
}

#' Generate an alt text from a plot
#'
#' This function returns a text that can be used as alt-text in webpages etc.
#' It will synthesize one from the information in the plot itself, but you can
#' add a conclusion to the synthesized text using `+ labs(alt_insight = <...>)`.
#'
#' There is no way an automatically generated description can compete with one
#' written by a human with knowledge of what the plot shows and in which
#' context. We urge users to write their own alt text if at all possible.
#' Guidance to how an effective alt-text is written can be found in
#' [Writing Alt Text for Data Visualization](https://medium.com/nightingale/writing-alt-text-for-data-visualization-2a218ef43f81)
#' and [Effective Practices for Description of Science Content within Digital Talking Books](https://www.wgbh.org/foundation/ncam/guidelines/effective-practices-for-description-of-science-content-within-digital-talking-books)
#'
#' @param p a ggplot object
#'
#' @return A text string
#'
#' @noRd
#'
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#'
#' get_alt_text(p)
#'
#' p <- p + ggtitle("The relationship between displacement and yield in cars")
#' get_alt_text(p)
#'
#' # It will use scale information if available
#' p <- p + scale_x_continuous("highway miles per gallon")
#' get_alt_text(p)
#'
#' # Add a short description of the main conclusion of the plot
#' p <- p + labs(alt_insight = "The higher the yield, the lower the displacement")
#' get_alt_text(p)
#'
#' # A user provided alt text takes precedence
#' p <- p + labs(
#'   alt = paste("A scatterplot showing the negative correlation between engine",
#'               "displacement as a function of highway miles per gallon")
#' )
#'
#' get_alt_text(p)
#'
generate_alt_text <- function(p) {
  # Combine titles
  title <- glue(glue_collapse(
    sub("\\.?$", "", c(p$labels$title, p$labels$subtitle)),
    last = ": "
  ), ". ")
  title <- safe_string(title)

  # Get axes descriptions
  axes <- glue(" showing ", glue_collapse(
    c(scale_description(p, "x"), scale_description(p, "y")),
    last = " and "
  ))
  axes <- safe_string(axes)

  # Get layer types
  layers <- vapply(p$layers, function(l) snake_class(l$geom), character(1))
  layers <- sub("_", " ", sub("^geom_", "", unique(layers)))
  layers <- glue(
    " using ",
    if (length(layers) == 1) "a " else "",
    glue_collapse(layers, sep = ", ", last = " and "),
    " layer",
    if (length(layers) == 1) "" else "s",
  )
  layers <- safe_string(layers)

  # Combine
  alt <- glue_collapse(
    c(glue("{title}A plot{axes}{layers}"), p$labels$alt_insight),
    last = ". "
  )
  as.character(alt)
}
safe_string <- function(string) {
  if (length(string) == 0) "" else string
}
scale_description <- function(p, name) {
  scale <- p$scales$get_scales(name)
  if (is.null(scale)) {
    lab <- p$labels[[name]]
    type <- "the"
  } else {
    lab <- scale$make_title(scale$name %|W|% p$labels[[name]])
    type <- "a continuous"
    if (scale$is_discrete()) type <- "a discrete"
    if (inherits(scale, "ScaleBinned")) type <- "a binned"
  }
  if (is.null(lab)) {
    return(NULL)
  }
  glue("{lab} on {type} {name}-axis")
}
