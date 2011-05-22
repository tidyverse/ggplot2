#' Textual annotations.
#' 
#' @name geom_text
#' @param parse If TRUE, the labels will be parsed into expressions and
#'   displayed as described in ?plotmath
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))
#' 
#' p + geom_text()
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
#'   parse = T)
#' 
#' # Use qplot instead
#' qplot(wt, mpg, data = mtcars, label = rownames(mtcars),
#'    geom=c("point", "text"))
#' qplot(wt, mpg, data = mtcars, label = rownames(mtcars), size = wt) +
#'   geom_text(colour = "red")
#'
#' # You can specify fontfamily, fontface and lineheight
#' p <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))
#' p + geom_text(fontface=3)
#' p + geom_text(aes(fontface=am+1))
#' p + geom_text(aes(family=c("serif", "mono")[am+1]))
GeomText <- proto(Geom, {
  objname <- "text"

  draw_groups <- function(., ...) .$draw(...)
  draw <- function(., data, scales, coordinates, ..., parse = FALSE) {
    
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

  
  icon <- function(.) textGrob("text", rot=45, gp=gpar(cex=1.2))
  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y", "label")
  default_aes <- function(.) aes(colour="black", size=5 , angle=0, hjust=0.5,
    vjust=0.5, alpha = 1, family="", fontface=1, lineheight=1.2)
  guide_geom <- function(x) "text"
  
})
