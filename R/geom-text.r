#' Textual annotations.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "text")}
#'
#' @section Alignment:
#' You can modify text alignment with the \code{vjust} and \code{hjust}
#' aesthetics. These can either be a number between 0 (right/bottom) and
#' 1 (top/left) or a character ("left", "middle", "right", "bottom", "center",
#' "top"). There are two special alignments: "inward" and "outward".
#' Inward always aligns text towards the center, and outward aligns
#' it away from the center
#'
#' @inheritParams geom_point
#' @param parse If TRUE, the labels will be parsed into expressions and
#'   displayed as described in ?plotmath
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#'   Useful for offseting text from points, particularly on discrete scales.
#' @param check_overlap If \code{TRUE}, text that overlaps previous text in the
#'   same layer will not be plotted. A quick and dirty way
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars)))
#'
#' p + geom_text()
#' # Avoid overlaps
#' p + geom_text(check_overlap = TRUE)
#' # Change size of the label
#' p + geom_text(size = 10)
#'
#' # Set aesthetics to fixed value
#' p + geom_point() + geom_text(hjust = 0, nudge_x = 0.05)
#' p + geom_point() + geom_text(vjust = 0, nudge_y = 0.5)
#' p + geom_point() + geom_text(angle = 45)
#' \dontrun{
#' p + geom_text(family = "Times New Roman")
#' }
#'
#' # Add aesthetic mappings
#' p + geom_text(aes(colour = factor(cyl)))
#' p + geom_text(aes(colour = factor(cyl))) +
#'   scale_colour_discrete(l = 40)
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
geom_text <- function(mapping = NULL, data = NULL, stat = "identity",
  position = "identity", parse = FALSE, show_guide = NA, inherit.aes = TRUE,
  ..., nudge_x = 0, nudge_y = 0, check_overlap = FALSE)
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
    show_guide = show_guide,
    inherit.aes = inherit.aes,
    geom_params = list(
      parse = parse,
      check_overlap = check_overlap
    ),
    params = list(...)
  )
}


GeomText <- proto2("GeomText", Geom,
  draw_groups = function(self, ...) self$draw(...),

  draw = function(data, scales, coordinates, ..., parse = FALSE,
                   na.rm = FALSE, check_overlap = FALSE) {
    data <- remove_missing(data, na.rm,
      c("x", "y", "label"), name = "geom_text")

    lab <- data$label
    if (parse) {
      lab <- parse(text = lab)
    }

    coords <- coord_transform(coordinates, data, scales)
    if (is.character(coords$vjust)) {
      coords$vjust <- compute_just(coords$vjust, coords$y)
    }
    if (is.character(coords$hjust)) {
      coords$hjust <- compute_just(coords$hjust, coords$x)
    }

    textGrob(
      lab,
      coords$x, coords$y, default.units = "native",
      hjust = coords$hjust, vjust = coords$vjust,
      rot = coords$angle,
      gp = gpar(
        col = alpha(coords$colour, coords$alpha),
        fontsize = coords$size * .pt,
        fontfamily = coords$family,
        fontface = coords$fontface,
        lineheight = coords$lineheight
      ),
      check.overlap = check_overlap
    )
  },

  required_aes = c("x", "y", "label"),

  default_aes = aes(colour = "black", size = 5, angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2),

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
