#' Textual annotations.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "text")}
#'
#' @inheritParams geom_point
#' @param parse If TRUE, the labels will be parsed into expressions and
#'   displayed as described in ?plotmath
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars)))
#'
#' p + geom_text()
#' # Change size of the label
#' p + geom_text(size=10)
#'
#' # Set aesthetics to fixed value
#' p + geom_point() + geom_text(hjust = 0, vjust = 0)
#' p + geom_point() + geom_text(angle = 45)
#' p + geom_text(family = "Times New Roman")
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
geom_text <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", parse = FALSE, ...) {
  GeomText$new(mapping = mapping, data = data, stat = stat, position = position,
    parse = parse, ...)
}

GeomText <- proto(Geom, {
  objname <- "text"

  draw_groups <- function(., ...) .$draw(...)

  draw <- function(., data, scales, coordinates, ..., parse = FALSE, na.rm = FALSE) {
    data <- remove_missing(data, na.rm,
      c("x", "y", "label"), name = "geom_text")

    lab <- data$label
    if (parse) {
      lab <- parse(text = lab)
    }

    coords <- coord_transform(coordinates, data, scales)
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
      )
    )
  }

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))
    textGrob(
      "a", 0.5, 0.5,
      rot = data$angle,
      gp = gpar(
        col = alpha(data$colour, data$alpha),
        fontsize = data$size * .pt
      )
    )
  }


  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y", "label")
  default_aes <- function(.) aes(colour = "black", size = 5, angle = 0,
    hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1,
    lineheight = 1.2)
  guide_geom <- function(x) "text"

})
