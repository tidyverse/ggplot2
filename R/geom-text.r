#' Textual annotations.
#'
#' \code{geom_text} adds text directly to the plot. \code{geom_label} draws
#' a rectangle underneath the text, making it easier to read.
#'
#' Note the the "width" and "height" of a text element are 0, so stacking
#' and dodging text will not work by default, and axis limits are not
#' automatically expanded to include all text. Obviously, labels do have
#' height and width, but they are physical units, not data units. The amount of
#' space they occupy on that plot is not constant in data units: when you
#' resize a plot, labels stay the same size, but the size of the axes changes.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "text")}
#'
#' @section \code{geom_label}:
#' Currently \code{geom_label} does not support the \code{rot} parameter and
#' is considerably slower than \code{geom_text}. The \code{fill} aesthetic
#' controls the background colour of the label.
#'
#' @section Alignment:
#' You can modify text alignment with the \code{vjust} and \code{hjust}
#' aesthetics. These can either be a number between 0 (right/bottom) and
#' 1 (top/left) or a character ("left", "middle", "right", "bottom", "center",
#' "top"). There are two special alignments: "inward" and "outward".
#' Inward always aligns text towards the center, and outward aligns
#' it away from the center
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @param parse If TRUE, the labels will be parsed into expressions and
#'   displayed as described in ?plotmath
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#'   Useful for offsetting text from points, particularly on discrete scales.
#' @param check_overlap If \code{TRUE}, text that overlaps previous text in the
#'   same layer will not be plotted. A quick and dirty way
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
#' p + geom_point() + geom_text(hjust = 0, nudge_x = 0.05)
#' p + geom_point() + geom_text(vjust = 0, nudge_y = 0.5)
#' p + geom_point() + geom_text(angle = 45)
#' \dontrun{
#' # Doesn't work on all systems
#' p + geom_text(family = "Times New Roman")
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
#' p + geom_text(aes(size = wt)) + scale_radius(range = c(3,6))
#'
#' # You can display expressions by setting parse = TRUE.  The
#' # details of the display are described in ?plotmath, but note that
#' # geom_text uses strings, not expressions.
#' p + geom_text(aes(label = paste(wt, "^(", cyl, ")", sep = "")),
#'   parse = TRUE)
#'
#' # Add a text annotation
#' p +
#'   geom_text() +
#'   annotate("text", label = "plot mpg vs. wt", x = 2, y = 15, size = 8, colour = "red")
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
#' ggplot(data = df, aes(x, y, fill = grp, label = y)) +
#'   geom_bar(stat = "identity", position = "dodge") +
#'   geom_text(position = "dodge")
#' # So tell it:
#' ggplot(data = df, aes(x, y, fill = grp, label = y)) +
#'   geom_bar(stat = "identity", position = "dodge") +
#'   geom_text(position = position_dodge(0.9))
#' # Use you can't nudge and dodge text, so instead adjust the y postion
#' ggplot(data = df, aes(x, y, fill = grp, label = y)) +
#'   geom_bar(stat = "identity", position = "dodge") +
#'   geom_text(aes(y = y + 0.05), position = position_dodge(0.9), vjust = 0)
#'
#' # To place text in the middle of each bar in a stacked barplot, you
#' # need to do the computation yourself
#' df <- transform(df, mid_y = ave(df$y, df$x, FUN = function(val) cumsum(val) - (0.5 * val)))
#'
#' ggplot(data = df, aes(x, y, fill = grp, label = y)) +
#'  geom_bar(stat = "identity") +
#'  geom_text(aes(y = mid_y))
#'
#' # Justification -------------------------------------------------------------
#' df <- data.frame(
#'   x = c(1, 1, 2, 2, 1.5),
#'   y = c(1, 2, 1, 2, 1.5),
#'   text = c("bottom-left", "bottom-right", "top-left", "top-right", "center")
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
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
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
    params = list(
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

  default_aes = aes(
    colour = "black", size = 3.88, angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),

  draw_panel = function(data, panel_scales, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE) {
    lab <- data$label
    if (parse) {
      lab <- parse(text = as.character(lab))
    }

    data <- coord$transform(data, panel_scales)
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
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

compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(left = 0, center = 0.5, right = 1,
    bottom = 0, middle = 0.5, top = 1)[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}
