#' Marginal rug plots.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "rug")}
#'
#' @inheritParams geom_point
#' @param sides A string that controls which sides of the plot the rugs appear on.
#'   It can be set to a string containing any of \code{"trbl"}, for top, right,
#'   bottom, and left.
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(x=wt, y=mpg))
#' p + geom_point()
#' p + geom_point() + geom_rug()
#' p + geom_point() + geom_rug(sides="b")    # Rug on bottom only
#' p + geom_point() + geom_rug(sides="trbl") # All four sides
#' p + geom_point() + geom_rug(position='jitter')
geom_rug <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", sides = "bl", ...) {
  GeomRug$new(mapping = mapping, data = data, stat = stat, position = position, sides = sides, ...)
}

GeomRug <- proto(Geom, {
  objname <- "rug"

  draw <- function(., data, scales, coordinates, sides, ...) {
    rugs <- list()
    data <- coord_transform(coordinates, data, scales)
    if (!is.null(data$x)) {
      if(grepl("b", sides)) {
        rugs$x_b <- segmentsGrob(
          x0 = unit(data$x, "native"), x1 = unit(data$x, "native"),
          y0 = unit(0, "npc"), y1 = unit(0.03, "npc"),
          gp = gpar(col = alpha(data$colour, data$alpha), lty = data$linetype, lwd = data$size * .pt)
        )
      }

      if(grepl("t", sides)) {
        rugs$x_t <- segmentsGrob(
          x0 = unit(data$x, "native"), x1 = unit(data$x, "native"),
          y0 = unit(1, "npc"), y1 = unit(0.97, "npc"),
          gp = gpar(col = alpha(data$colour, data$alpha), lty = data$linetype, lwd = data$size * .pt)
        )
      }
    }

    if (!is.null(data$y)) {
      if(grepl("l", sides)) {
        rugs$y_l <- segmentsGrob(
          y0 = unit(data$y, "native"), y1 = unit(data$y, "native"),
          x0 = unit(0, "npc"), x1 = unit(0.03, "npc"),
          gp = gpar(col = alpha(data$colour, data$alpha), lty = data$linetype, lwd = data$size * .pt)
        )
      }

      if(grepl("r", sides)) {
        rugs$y_r <- segmentsGrob(
          y0 = unit(data$y, "native"), y1 = unit(data$y, "native"),
          x0 = unit(1, "npc"), x1 = unit(0.97, "npc"),
          gp = gpar(col = alpha(data$colour, data$alpha), lty = data$linetype, lwd = data$size * .pt)
        )
      }
    }

    gTree(children = do.call("gList", rugs))
  }

  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  guide_geom <- function(.) "path"
})
