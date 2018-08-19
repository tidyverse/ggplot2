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
#' p + labs(title = "title") + labs(title = NULL)
labs <- function(..., title = waiver(), subtitle = waiver(), caption = waiver(), tag = waiver()) {
  args <- rlang::list2(..., title = title, subtitle = subtitle, caption = caption, tag = tag)

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
