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
#' \donttest{
#' p <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))
#'
#' p + geom_text()
#' # Change size of the label
#' p + geom_text(size=10)
#' p <- p + geom_point()
#'
#' # Set aesthetics to fixed value
#' p + geom_text()
#' p + geom_point() + geom_text(hjust=0, vjust=0)
#' p + geom_point() + geom_text(angle = 45)
#'
#' # Add aesthetic mappings
#' p + geom_text(aes(colour=factor(cyl)))
#' p + geom_text(aes(colour=factor(cyl))) + scale_colour_discrete(l=40)
#'
#' p + geom_text(aes(size=wt))
#' p + geom_text(aes(size=wt)) + scale_size(range=c(3,6))
#'
#' # You can display expressions by setting parse = TRUE.  The
#' # details of the display are described in ?plotmath, but note that
#' # geom_text uses strings, not expressions.
#' p + geom_text(aes(label = paste(wt, "^(", cyl, ")", sep = "")),
#'   parse = TRUE)
#'
#' # Add an annotation not from a variable source
#' c <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' c + geom_text(data = NULL, x = 5, y = 30, label = "plot mpg vs. wt")
#' # Or, you can use annotate
#' c + annotate("text", label = "plot mpg vs. wt", x = 2, y = 15, size = 8, colour = "red")
#'
#' # Use qplot instead
#' qplot(wt, mpg, data = mtcars, label = rownames(mtcars),
#'    geom=c("point", "text"))
#' qplot(wt, mpg, data = mtcars, label = rownames(mtcars), size = wt) +
#'   geom_text(colour = "red")
#'
#' # You can specify family, fontface and lineheight
#' p <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))
#' p + geom_text(fontface=3)
#' p + geom_text(aes(fontface=am+1))
#' p + geom_text(aes(family=c("serif", "mono")[am+1]))
#' }
geom_text <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
parse = FALSE, ...) {
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

    with(coord_transform(coordinates, data, scales),
      textGrob(lab, x, y, default.units="native",
        hjust=hjust, vjust=vjust, rot=angle,
        gp = gpar(col = alpha(colour, alpha), fontsize = size * .pt,
          fontfamily = family, fontface = fontface, lineheight = lineheight))
    )
  }

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))
    with(data,
      textGrob("a", 0.5, 0.5, rot = angle,
      gp=gpar(col=alpha(colour, alpha), fontsize = size * .pt))
    )
  }


  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y", "label")
  default_aes <- function(.) aes(colour="black", size=5 , angle=0, hjust=0.5,
    vjust=0.5, alpha = NA, family="", fontface=1, lineheight=1.2)
  guide_geom <- function(x) "text"

})
