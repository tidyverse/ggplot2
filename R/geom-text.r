#' Text
#'
#' Text geoms are useful for labeling plots. They can be used by themselves as
#' scatterplots or in combination with other geoms, for example, for labeling
#' points or for annotating the height of bars. `geom_text()` adds only text
#' to the plot. `geom_label()` draws a rectangle behind the text, making it
#' easier to read.
#'
#' Note that when you resize a plot, text labels stay the same size, even
#' though the size of the plot area changes. This happens because the "width"
#' and "height" of a text element are 0. Obviously, text labels do have height
#' and width, but they are physical units, not data units. For the same reason,
#' stacking and dodging text will not work by default, and axis limits are not
#' automatically expanded to include all text.
#'
#' `geom_text()` and `geom_label()` add labels for each row in the
#' data, even if coordinates x, y are set to single values in the call
#' to `geom_label()` or `geom_text()`.
#' To add labels at specified points use [annotate()] with
#' `annotate(geom = "text", ...)` or `annotate(geom = "label", ...)`.
#'
#' To automatically position non-overlapping text labels see the
#' \href{https://cran.r-project.org/package=ggrepel}{ggrepel}
#' package.
#'
#' @eval rd_aesthetics("geom", "text")
#' @section `geom_label()`:
#' Currently `geom_label()` does not support the `check_overlap` argument
#' or the `angle` aesthetic. Also, it is considerably slower than `geom_text()`.
#' The `fill` aesthetic controls the background colour of the label.
#'
#' @section Alignment:
#' You can modify text alignment with the `vjust` and `hjust`
#' aesthetics. These can either be a number between 0 (right/bottom) and
#' 1 (top/left) or a character (`"left"`, `"middle"`, `"right"`, `"bottom"`,
#' `"center"`, `"top"`). There are two special alignments: `"inward"` and
#' `"outward"`. Inward always aligns text towards the center, and outward
#' aligns it away from the center.
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @param parse If `TRUE`, the labels will be parsed into expressions and
#'   displayed as described in `?plotmath`.
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#'   Useful for offsetting text from points, particularly on discrete scales.
#'   Cannot be jointly specified with `position`.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function. Cannot be jointy specified with
#'  `nudge_x` or `nudge_y`.
#' @param check_overlap If `TRUE`, text that overlaps previous text in the
#'   same layer will not be plotted. `check_overlap` happens at draw time and in
#'   the order of the data. Therefore data should be arranged by the label
#'   column before calling `geom_text()`. Note that this argument is not
#'   supported by `geom_label()`.
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars)))
#'
#' p + geom_text()
#' # Avoid overlaps
#' p + geom_text(check_overlap = TRUE)
#' # Labels with background
#' p + geom_label()
#' # Change size of the label
#' p + geom_text(size = 10)
#'
#' # Set aesthetics to fixed value
#' p +
#'   geom_point() +
#'   geom_text(hjust = 0, nudge_x = 0.05)
#' p +
#'   geom_point() +
#'   geom_text(vjust = 0, nudge_y = 0.5)
#' p +
#'   geom_point() +
#'   geom_text(angle = 45)
#' \dontrun{
#' # Doesn't work on all systems
#' p +
#'   geom_text(family = "Times New Roman")
#' }
#'
#' # Add aesthetic mappings
#' p + geom_text(aes(colour = factor(cyl)))
#' p + geom_text(aes(colour = factor(cyl))) +
#'   scale_colour_discrete(l = 40)
#' p + geom_label(aes(fill = factor(cyl)), colour = "white", fontface = "bold")
#'
#' p + geom_text(aes(size = wt))
#' # Scale height of text, rather than sqrt(height)
#' p +
#'   geom_text(aes(size = wt)) +
#'   scale_radius(range = c(3,6))
#'
#' # You can display expressions by setting parse = TRUE.  The
#' # details of the display are described in ?plotmath, but note that
#' # geom_text uses strings, not expressions.
#' p +
#'   geom_text(
#'     aes(label = paste(wt, "^(", cyl, ")", sep = "")),
#'     parse = TRUE
#'   )
#'
#' # Add a text annotation
#' p +
#'   geom_text() +
#'   annotate(
#'     "text", label = "plot mpg vs. wt",
#'     x = 2, y = 15, size = 8, colour = "red"
#'   )
#'
#' \donttest{
#' # Aligning labels and bars --------------------------------------------------
#' df <- data.frame(
#'   x = factor(c(1, 1, 2, 2)),
#'   y = c(1, 3, 2, 1),
#'   grp = c("a", "b", "a", "b")
#' )
#'
#' # ggplot2 doesn't know you want to give the labels the same virtual width
#' # as the bars:
#' ggplot(data = df, aes(x, y, group = grp)) +
#'   geom_col(aes(fill = grp), position = "dodge") +
#'   geom_text(aes(label = y), position = "dodge")
#' # So tell it:
#' ggplot(data = df, aes(x, y, group = grp)) +
#'   geom_col(aes(fill = grp), position = "dodge") +
#'   geom_text(aes(label = y), position = position_dodge(0.9))
#' # Use you can't nudge and dodge text, so instead adjust the y position
#' ggplot(data = df, aes(x, y, group = grp)) +
#'   geom_col(aes(fill = grp), position = "dodge") +
#'   geom_text(
#'     aes(label = y, y = y + 0.05),
#'     position = position_dodge(0.9),
#'     vjust = 0
#'   )
#'
#' # To place text in the middle of each bar in a stacked barplot, you
#' # need to set the vjust parameter of position_stack()
#' ggplot(data = df, aes(x, y, group = grp)) +
#'  geom_col(aes(fill = grp)) +
#'  geom_text(aes(label = y), position = position_stack(vjust = 0.5))
#'
#' # Justification -------------------------------------------------------------
#' df <- data.frame(
#'   x = c(1, 1, 2, 2, 1.5),
#'   y = c(1, 2, 1, 2, 1.5),
#'   text = c("bottom-left", "top-left", "bottom-right", "top-right", "center")
#' )
#' ggplot(df, aes(x, y)) +
#'   geom_text(aes(label = text))
#' ggplot(df, aes(x, y)) +
#'   geom_text(aes(label = text), vjust = "inward", hjust = "inward")
#' }
geom_text <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      parse = FALSE,
                      nudge_x = 0,
                      nudge_y = 0,
                      check_overlap = FALSE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      cli::cli_abort(c(
        "both {.arg position} and {.arg nudge_x}/{.arg nudge_y} are supplied",
        "i" = "Only use one approach to alter the position"
      ))
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomText <- ggproto("GeomText", Geom,
  required_aes = c("x", "y", "label"),

  non_missing_aes = "angle",

  default_aes = aes(
    colour = "black", size = 3.88, angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),

  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE) {
    lab <- data$label
    if (parse) {
      lab <- parse_safe(as.character(lab))
    }

    data <- coord$transform(data, panel_params)

    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y, data$x, data$angle)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x, data$y, data$angle)
    }

    textGrob(
      lab,
      data$x, data$y, default.units = "native",
      hjust = data$hjust, vjust = data$vjust,
      rot = data$angle,
      gp = gpar(
        col = alpha(data$colour, data$alpha),
        fontsize = data$size * .pt,
        fontfamily = data$family,
        fontface = data$fontface,
        lineheight = data$lineheight
      ),
      check.overlap = check_overlap
    )
  },

  draw_key = draw_key_text
)

compute_just <- function(just, a, b = a, angle = 0) {
  #  As justification direction is relative to the text, not the plotting area
  #  we need to swap x and y if text direction is rotated so that hjust is
  #  applied along y and vjust along x.
  if (any(grepl("outward|inward", just))) {
    # ensure all angles are in -360...+360
    angle <- angle %% 360
    # ensure correct behaviour for angles in -360...+360
    angle <- ifelse(angle > 180, angle - 360, angle)
    angle <- ifelse(angle < -180, angle + 360, angle)
    rotated_forward <-
      grepl("outward|inward", just) & (angle > 45 & angle < 135)
    rotated_backwards <-
      grepl("outward|inward", just) & (angle < -45 & angle > -135)

    ab <- ifelse(rotated_forward | rotated_backwards, b, a)
    just_swap <- rotated_backwards | abs(angle) > 135
    inward <-
      (just == "inward" & !just_swap | just == "outward" & just_swap)
    just[inward] <- c("left", "middle", "right")[just_dir(ab[inward])]
    outward <-
      (just == "outward" & !just_swap) | (just == "inward" & just_swap)
    just[outward] <- c("right", "middle", "left")[just_dir(ab[outward])]

  }

  unname(c(left = 0, center = 0.5, right = 1,
    bottom = 0, middle = 0.5, top = 1)[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}
