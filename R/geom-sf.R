#' @export
#' @rdname ggsf
#' @usage NULL
#' @format NULL
GeomSf <- ggproto("GeomSf", Geom,
                  required_aes = "geometry",
                  default_aes = aes(
                    shape = NULL,
                    colour = NULL,
                    fill = NULL,
                    size = NULL,
                    linetype = 1,
                    alpha = NA,
                    stroke = 0.5
                  ),

  draw_panel = function(data, panel_params, coord, legend = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10) {
    if (!inherits(coord, "CoordSf")) {
      stop("geom_sf() must be used with coord_sf()", call. = FALSE)
    }

    # Need to refactor this to generate one grob per geometry type
    coord <- coord$transform(data, panel_params)
    grobs <- lapply(1:nrow(data), function(i) {
      sf_grob(
        coord[i, , drop = FALSE],
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre
      )
    })
    do.call("gList", grobs)
  },

  draw_key = function(data, params, size) {
    data <- modify_list(default_aesthetics(params$legend), data)
    if (params$legend == "point") {
      draw_key_point(data, params, size)
    } else if (params$legend == "line") {
      draw_key_path(data, params, size)
    } else {
      draw_key_polygon(data, params, size)
    }
  }
)

default_aesthetics <- function(type) {
  if (type == "point") {
    GeomPoint$default_aes
  } else if (type == "line") {
    GeomLine$default_aes
  } else  {
    modify_list(GeomPolygon$default_aes, list(fill = "grey90", colour = "grey35"))
  }
}

sf_grob <- function(row, lineend = "butt", linejoin = "round", linemitre = 10) {
  # Need to extract geometry out of corresponding list column
  geometry <- row$geometry[[1]]

  if (inherits(geometry, c("POINT", "MULTIPOINT"))) {
    row <- modify_list(default_aesthetics("point"), row)
    gp <- gpar(
      col = alpha(row$colour, row$alpha),
      fill = alpha(row$fill, row$alpha),
      # Stroke is added around the outside of the point
      fontsize = row$size * .pt + row$stroke * .stroke / 2,
      lwd = row$stroke * .stroke / 2
    )
    sf::st_as_grob(geometry, gp = gp, pch = row$shape)
  } else {
    row <- modify_list(default_aesthetics("poly"), row)
    gp <- gpar(
      col = row$colour,
      fill = alpha(row$fill, row$alpha),
      lwd = row$size * .pt,
      lty = row$linetype,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre
    )
    sf::st_as_grob(geometry, gp = gp)
  }
}

#' @export
#' @rdname ggsf
#' @inheritParams geom_point
geom_sf <- function(mapping = aes(), data = NULL, stat = "sf",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  c(
    layer(
      geom = GeomSf,
      data = data,
      mapping = mapping,
      stat = stat,
      position = position,
      show.legend = if (is.character(show.legend)) TRUE else show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        legend = if (is.character(show.legend)) show.legend else "polygon",
        ...
      ),
      layer_class = LayerSf
    ),
    coord_sf(default = TRUE)
  )
}

#' @export
#' @rdname ggsf
#' @inheritParams geom_label
#' @inheritParams stat_sf_coordinates
geom_sf_label <- function(mapping = aes(), data = NULL,
                          stat = "sf_coordinates", position = "identity",
                          ...,
                          parse = FALSE,
                          nudge_x = 0,
                          nudge_y = 0,
                          label.padding = unit(0.25, "lines"),
                          label.r = unit(0.15, "lines"),
                          label.size = 0.25,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          fun.geometry = NULL) {

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
    geom = GeomLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      na.rm = na.rm,
      fun.geometry = fun.geometry,
      ...
    ),
    layer_class = LayerSf
  )
}

#' @export
#' @rdname ggsf
#' @inheritParams geom_text
#' @inheritParams stat_sf_coordinates
geom_sf_text <- function(mapping = aes(), data = NULL,
                         stat = "sf_coordinates", position = "identity",
                         ...,
                         parse = FALSE,
                         nudge_x = 0,
                         nudge_y = 0,
                         check_overlap = FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         fun.geometry = NULL) {

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.", call. = FALSE)
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
      fun.geometry = fun.geometry,
      ...
    ),
    layer_class = LayerSf
  )
}
