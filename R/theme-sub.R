#' Shortcuts for theme settings
#'
#' This collection of functions serves as a shortcut for [`theme()`][theme] with
#' shorter argument names. Besides the shorter arguments, it also helps in
#' keeping theme declarations more organised.
#'
#' @eval subtheme_param_doc()
#'
#' @return A `theme`-class object that can be added to a plot.
#' @name subtheme
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mtcars, aes(disp, mpg, colour = drat)) +
#'   geom_point()
#'
#' red_text <- element_text(colour = "red")
#' red_line <- element_line(colour = "red")
#'
#' # The theme settings below:
#' p + theme(
#'   axis.title.x.bottom = red_text,
#'   axis.text.x.bottom  = red_text,
#'   axis.line.x.bottom  = red_line,
#'   axis.ticks.x.bottom = red_line
#' )
#'
#' # Are equivalent to these less verbose theme settings
#' p + theme_sub_axis_bottom(
#'   title = red_text,
#'   text  = red_text,
#'   line  = red_line,
#'   ticks = red_line
#' )
NULL

subtheme <- function(elements, prefix = "", suffix = "", call = caller_env()) {
  if (length(elements) < 1) {
    return(theme())
  }
  names(elements) <- paste0(prefix, names(elements), suffix)

  extra <- setdiff(names(elements), names(get_element_tree()))
  if (length(extra) > 0) {
    cli::cli_warn(
      "Ignoring unknown {.fn theme} element{?s}: {.and {.field {extra}}}.",
      call = call
    )
    elements <- elements[setdiff(names(elements), extra)]
  }

  exec(theme, !!!elements)
}

#' @export
#' @describeIn subtheme Theme specification for all axes.
theme_sub_axis <- function(title, text, ticks, ticks.length, line) {
  subtheme(find_args(), "axis.")
}

#' @export
#' @describeIn subtheme Theme specification for both x axes.
theme_sub_axis_x <- function(title, text, ticks, ticks.length, line) {
  subtheme(find_args(), "axis.", ".x")
}

#' @export
#' @describeIn subtheme Theme specification for both y axes.
theme_sub_axis_y <- function(title, text, ticks, ticks.length, line) {
  subtheme(find_args(), "axis.", ".y")
}

#' @export
#' @describeIn subtheme Theme specification for the bottom x axis.
theme_sub_axis_bottom <- function(title, text, ticks, ticks.length, line) {
  subtheme(find_args(), "axis.", ".x.bottom")
}

#' @export
#' @describeIn subtheme Theme specification for the top x axis.
theme_sub_axis_top <- function(title, text, ticks, ticks.length, line) {
  subtheme(find_args(), "axis.", ".x.top")
}

#' @export
#' @describeIn subtheme Theme specification for the left y axis.
theme_sub_axis_left <- function(title, text, ticks, ticks.length, line) {
  subtheme(find_args(), "axis.", ".y.left")
}

#' @export
#' @describeIn subtheme Theme specification for the right y axis.
theme_sub_axis_right <- function(title, text, ticks, ticks.length, line) {
  subtheme(find_args(), "axis.", ".y.right")
}

#' @export
#' @describeIn subtheme Theme specification for the legend.
theme_sub_legend <- function(background, margin, spacing, spacing.x, spacing.y,
                             key, key.size, key.height, key.width, text, title,
                             position, direction, justification, box, box.just,
                             box.margin, box.background, box.spacing) {
  subtheme(find_args(), "legend.")
}

#' @export
#' @describeIn subtheme Theme specification for the panels.
theme_sub_panel <- function(background, border, spacing, spacing.x, spacing.y,
                            grid, grid.major, grid.minor, grid.major.x,
                            grid.major.y, grid.minor.x, grid.minor.y, ontop) {
  subtheme(find_args(), "panel.")
}

#' @export
#' @describeIn subtheme Theme specification for the whole plot.
theme_sub_plot <- function(background, title, title.position, subtitle, caption,
                           caption.position, tag, tag.position, tag.location,
                           margin) {
  subtheme(find_args(), "plot.")
}

#' @export
#' @describeIn subtheme Theme specification for facet strips.
theme_sub_strip <- function(background, background.x, background.y, clip,
                            placement, text, text.x, text.x.bottom, text.x.top,
                            text.y, text.y.left, text.y.right,
                            switch.pad.grid, switch.pad.wrap) {
  subtheme(find_args(), "strip.")
}

subtheme_param_doc <- function() {
  funs <- list(
    theme_sub_axis, theme_sub_axis_x, theme_sub_axis_y, theme_sub_axis_bottom,
    theme_sub_axis_top, theme_sub_axis_left, theme_sub_axis_right, theme_sub_legend,
    theme_sub_panel, theme_sub_plot, theme_sub_strip
  )
  args <- sort(unique(unlist(lapply(funs, fn_fmls_names), use.names = FALSE)))
  paste0(
    "@param ",
    paste0(args, collapse = ","),
    " Arguments that are renamed and passed on to ",
    "\\code{\\link[=theme]{theme()}}."
  )
}
