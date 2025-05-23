# Default scales -------------------------------------------------------------

#' @export
#' @rdname scale_viridis
#' @usage NULL
scale_colour_ordinal <- function(..., type = getOption("ggplot2.ordinal.colour", getOption("ggplot2.ordinal.fill"))) {
  type <- type %||% scale_colour_viridis_d
  args <- list2(...)
  args$call <- args$call %||% current_call()
  if (is.function(type)) {
    if (any(c("...", "call") %in% fn_fmls_names(type))) {
      args$call <- args$call %||% current_call()
    }
    exec(type, !!!args)
  } else {
    exec(
      discrete_scale,
      aesthetics = "colour",
      palette = pal_ordinal(type),
      !!!args
    )
  }
}


#' @export
#' @rdname scale_viridis
#' @usage NULL
scale_color_ordinal <- scale_colour_ordinal

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_colour_datetime <- function(name = waiver(), ...,
                                  low = "#132B43",
                                  high = "#56B1F7",
                                  space = "Lab",
                                  na.value = "grey50",
                                  guide = "colourbar",
                                  aesthetics = "colour") {
  datetime_scale(
    aesthetics = aesthetics, transform = "time", name = name,
    palette = pal_seq_gradient(low, high, space),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_color_datetime <- scale_colour_datetime

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_colour_date <- function(name = waiver(),
                              ...,
                              low = "#132B43",
                              high = "#56B1F7",
                              space = "Lab",
                              na.value = "grey50",
                              guide = "colourbar",
                              aesthetics = "colour") {
  datetime_scale(
    aesthetics = aesthetics, transform = "date", name = name,
    palette = pal_seq_gradient(low, high, space),
    na.value = na.value,
    guide = guide,
    ...
  )
}


#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_color_date <- scale_colour_date

#' @export
#' @rdname scale_viridis
#' @usage NULL
scale_fill_ordinal <- function(..., type = getOption("ggplot2.ordinal.fill", getOption("ggplot2.ordinal.colour"))) {
  type <- type %||% scale_fill_viridis_d
  args <- list2(...)
  args$call <- args$call %||% current_call()

  if (is.function(type)) {
    if (any(c("...", "call") %in% fn_fmls_names(type))) {
      args$call <- args$call %||% current_call()
    }
    exec(type, !!!args)
  } else {
    exec(
      discrete_scale,
      aesthetics = "fill",
      palette = pal_ordinal(type),
      !!!args
    )
  }
}

pal_ordinal <- function(colours, na.color = "grey50", alpha = TRUE) {
  pal <- scales::colour_ramp(colours, na.color = na.color, alpha = alpha)
  function(n) {
    pal(seq(0, 1, length.out = n))
  }
}

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_fill_datetime <- function(name = waiver(), ...,
                                low = "#132B43",
                                high = "#56B1F7",
                                space = "Lab",
                                na.value = "grey50",
                                guide = "colourbar",
                                aesthetics = "fill") {
  datetime_scale(
    aesthetics = aesthetics, transform = "time", name = name,
    palette = pal_seq_gradient(low, high, space),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_fill_date <- function(name = waiver(), ...,
                            low = "#132B43",
                            high = "#56B1F7",
                            space = "Lab",
                            na.value = "grey50",
                            guide = "colourbar",
                            aesthetics = "fill") {
  datetime_scale(
    aesthetics = aesthetics, transform = "date", name = name,
    palette = pal_seq_gradient(low, high, space),
    na.value = na.value,
    guide = guide,
    ...
  )
}


# British to American spellings ----------------------------------------------

#' @export
#' @rdname scale_brewer
#' @usage NULL
scale_color_brewer <- scale_colour_brewer

#' @export
#' @rdname scale_brewer
#' @usage NULL
scale_color_distiller <- scale_colour_distiller

#' @export
#' @rdname scale_brewer
#' @usage NULL
scale_color_fermenter <- scale_colour_fermenter

#' @export
#' @rdname scale_colour_continuous
#' @usage NULL
scale_color_continuous <- scale_colour_continuous

#' @export
#' @rdname scale_colour_continuous
#' @usage NULL
scale_color_binned <- scale_colour_binned

#' @export
#' @rdname scale_colour_discrete
#' @usage NULL
scale_color_discrete <- scale_colour_discrete

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_color_gradient <- scale_colour_gradient

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_color_gradient2 <- scale_colour_gradient2

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_color_gradientn <- scale_colour_gradientn

#' @export
#' @rdname scale_steps
#' @usage NULL
scale_color_steps <- scale_colour_steps

#' @export
#' @rdname scale_steps
#' @usage NULL
scale_color_steps2 <- scale_colour_steps2

#' @export
#' @rdname scale_steps
#' @usage NULL
scale_color_stepsn <- scale_colour_stepsn

#' @export
#' @rdname scale_grey
#' @usage NULL
scale_color_grey <- scale_colour_grey

#' @export
#' @rdname scale_hue
#' @usage NULL
scale_color_hue <- scale_colour_hue

#' @export
#' @rdname scale_identity
#' @usage NULL
scale_color_identity <- scale_colour_identity

#' @export
#' @rdname scale_manual
#' @usage NULL
scale_color_manual <- scale_colour_manual

#' @export
#' @rdname scale_viridis
#' @usage NULL
scale_color_viridis_d <- scale_colour_viridis_d

#' @export
#' @rdname scale_viridis
#' @usage NULL
scale_color_viridis_c <- scale_colour_viridis_c

#' @export
#' @rdname scale_viridis
#' @usage NULL
scale_color_viridis_b <- scale_colour_viridis_b
