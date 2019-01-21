#' Rug plots in the margins
#'
#' A rug plot is a compact visualisation designed to supplement a 2d display
#' with the two 1d marginal distributions. Rug plots display individual
#' cases so are best used with smaller datasets.
#'
#' The rug lines are drawn with a fixed size (3\% of the total plot size) so
#' are dependent on the overall scale expansion in order not to overplot
#' existing data.
#'
#' @eval rd_aesthetics("geom", "rug")
#' @inheritParams layer
#' @inheritParams geom_point
#' @param sides A string that controls which sides of the plot the rugs appear on.
#'   It can be set to a string containing any of `"trbl"`, for top, right,
#'   bottom, and left.
#' @param outside logical that controls whether to move the rug tassels outside of the plot area. Default is off (FALSE). You will also need to use `coord_cartesian(clip = "off")`. When set to TRUE, also consider changing the sides argument to "tr". See examples.
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point()
#' p
#' p + geom_rug()
#' p + geom_rug(sides="b")    # Rug on bottom only
#' p + geom_rug(sides="trbl") # All four sides
#'
#' # Use jittering to avoid overplotting for smaller datasets
#' ggplot(mpg, aes(displ, cty)) +
#'   geom_point() +
#'   geom_rug()
#'
#' ggplot(mpg, aes(displ, cty)) +
#'   geom_jitter() +
#'   geom_rug(alpha = 1/2, position = "jitter")
#'
#' # move the rug tassels to outside the plot
#' # remember to set clip = "off".
#' p + geom_rug(outside = TRUE) +
#'   coord_cartesian(clip = "off")
#'
#' # set sides to top right, and then move the margins
#' p + geom_rug(outside = TRUE, sides = "tr") +
#'    coord_cartesian(clip = "off") +
#'    theme(plot.margin = margin(1, 1, 1, 1, "cm"))
#'
geom_rug <- function(mapping = NULL, data = NULL,
                     stat = "identity", position = "identity",
                     ...,
                     outside = FALSE,
                     sides = "bl",
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRug,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outside = outside,
      sides = sides,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomRug <- ggproto("GeomRug", Geom,
  optional_aes = c("x", "y"),

  draw_panel = function(data, panel_params, coord, sides = "bl", outside) {
    rugs <- list()
    data <- coord$transform(data, panel_params)

    # For coord_flip, coord$tranform does not flip the sides where to
    # draw the rugs. We have to flip them.
    if (inherits(coord, 'CoordFlip')) {
      sides <- chartr('tblr', 'rlbt', sides)
    }

    # move the rug to outside the main plot space
    rug_length <- if (!outside) {
      list(min = 0.03, max = 0.97)
    } else {
      list(min = -0.03, max = 1.03)
    }

    gp <- gpar(col = alpha(data$colour, data$alpha), lty = data$linetype, lwd = data$size * .pt)
    if (!is.null(data$x)) {
      if (grepl("b", sides)) {
        rugs$x_b <- segmentsGrob(
          x0 = unit(data$x, "native"), x1 = unit(data$x, "native"),
          y0 = unit(0, "npc"), y1 = unit(rug_length$min, "npc"),
          gp = gp
        )
      }

      if (grepl("t", sides)) {
        rugs$x_t <- segmentsGrob(
          x0 = unit(data$x, "native"), x1 = unit(data$x, "native"),
          y0 = unit(1, "npc"), y1 = unit(rug_length$max, "npc"),
          gp = gp
        )
      }
    }

    if (!is.null(data$y)) {
      if (grepl("l", sides)) {
        rugs$y_l <- segmentsGrob(
          y0 = unit(data$y, "native"), y1 = unit(data$y, "native"),
          x0 = unit(0, "npc"), x1 = unit(rug_length$min, "npc"),
          gp = gp
        )
      }

      if (grepl("r", sides)) {
        rugs$y_r <- segmentsGrob(
          y0 = unit(data$y, "native"), y1 = unit(data$y, "native"),
          x0 = unit(1, "npc"), x1 = unit(rug_length$max, "npc"),
          gp = gp
        )
      }
    }

    gTree(children = do.call("gList", rugs))
  },

  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),

  draw_key = draw_key_path
)
