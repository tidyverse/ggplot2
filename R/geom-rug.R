#' Rug plots in the margins
#'
#' A rug plot is a compact visualisation designed to supplement a 2d display
#' with the two 1d marginal distributions. Rug plots display individual
#' cases so are best used with smaller datasets.
#'
#' By default, the rug lines are drawn with a length that corresponds to 3%
#' of the total plot size. Since the default scale expansion of for continuous
#' variables is 5% at both ends of the scale, the rug will not overlap with
#' any data points under the default settings.
#'
#' @eval rd_aesthetics("geom", "rug")
#' @inheritParams layer
#' @inheritParams geom_point
#' @param sides A string that controls which sides of the plot the rugs appear on.
#'   It can be set to a string containing any of `"trbl"`, for top, right,
#'   bottom, and left.
#' @param outside logical that controls whether to move the rug tassels outside of the plot area. Default is off (FALSE). You will also need to use `coord_cartesian(clip = "off")`. When set to TRUE, also consider changing the sides argument to "tr". See examples.
#' @param length A [grid::unit()] object that sets the length of the rug lines. Use scale expansion to avoid overplotting of data.
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
#' p +
#'   geom_rug(outside = TRUE) +
#'   coord_cartesian(clip = "off")
#'
#' # set sides to top right, and then move the margins
#' p +
#'   geom_rug(outside = TRUE, sides = "tr") +
#'   coord_cartesian(clip = "off") +
#'   theme(plot.margin = margin(1, 1, 1, 1, "cm"))
#'
#' # increase the line length and
#' # expand axis to avoid overplotting
#' p +
#'   geom_rug(length = unit(0.05, "npc")) +
#'   scale_y_continuous(expand = c(0.1, 0.1))
#'
geom_rug <- function(mapping = NULL, data = NULL,
                     stat = "identity", position = "identity",
                     ...,
                     outside = FALSE,
                     sides = "bl",
                     length = unit(0.03, "npc"),
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
    params = list2(
      outside = outside,
      sides = sides,
      length = length,
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

  draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                        sides = "bl", outside = FALSE, length = unit(0.03, "npc")) {
    data <- check_linewidth(data, snake_class(self))
    check_inherits(length, "unit")
    rugs <- list()
    data <- coord$transform(data, panel_params)

    # For coord_flip, coord$transform does not flip the sides where to
    # draw the rugs. We have to flip them.
    if (inherits(coord, 'CoordFlip')) {
      sides <- chartr('tblr', 'rlbt', sides)
    }

    # move the rug to outside the main plot space
    rug_length <- if (!outside) {
      list(min = length, max = unit(1, "npc") - length)
    } else {
      list(min = -1 * length, max = unit(1, "npc") + length)
    }

    gp <- gg_par(
      col = alpha(data$colour, data$alpha),
      lty = data$linetype,
      lwd = data$linewidth,
      lineend = lineend
    )
    if (!is.null(data$x)) {
      if (grepl("b", sides)) {
        rugs$x_b <- segmentsGrob(
          x0 = unit(data$x, "native"), x1 = unit(data$x, "native"),
          y0 = unit(0, "npc"), y1 = rug_length$min,
          gp = gp
        )
      }

      if (grepl("t", sides)) {
        rugs$x_t <- segmentsGrob(
          x0 = unit(data$x, "native"), x1 = unit(data$x, "native"),
          y0 = unit(1, "npc"), y1 = rug_length$max,
          gp = gp
        )
      }
    }

    if (!is.null(data$y)) {
      if (grepl("l", sides)) {
        rugs$y_l <- segmentsGrob(
          y0 = unit(data$y, "native"), y1 = unit(data$y, "native"),
          x0 = unit(0, "npc"), x1 = rug_length$min,
          gp = gp
        )
      }

      if (grepl("r", sides)) {
        rugs$y_r <- segmentsGrob(
          y0 = unit(data$y, "native"), y1 = unit(data$y, "native"),
          x0 = unit(1, "npc"), x1 = rug_length$max,
          gp = gp
        )
      }
    }

    gTree(children = inject(gList(!!!rugs)))
  },

  default_aes = aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA),

  draw_key = draw_key_path,

  rename_size = TRUE
)
