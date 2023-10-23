#' @include scale-viridis.R

# Default scales -------------------------------------------------------------

#' @export
#' @rdname scale_viridis
#' @usage NULL
scale_colour_ordinal <- function(..., aesthetics = "colour", type = getOption("ggplot2.ordinal.colour")) {
  type <- type %||% scale_colour_viridis_d
  args <- list2(...)
  args$call <- args$call %||% current_call()
  if (is.function(type)) {
    if (any(c("...", "call") %in% fn_fmls_names(type))) {
      args$call <- args$call %||% current_call()
    }
    exec(type, !!!args, aesthetics = aesthetics)
  } else {
    exec(
      discrete_scale,
      aesthetics = aesthetics,
      palette = ordinal_pal(type),
      !!!args
    )
  }
}

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_colour_datetime <- function(...,
                                  aesthetics = "colour",
                                  low = "#132B43",
                                  high = "#56B1F7",
                                  space = "Lab",
                                  na.value = "grey50",
                                  guide = "colourbar") {
  datetime_scale(
    aesthetics,
    "time",
    palette = seq_gradient_pal(low, high, space),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @rdname scale_gradient
#' @usage NULL
scale_colour_date <- function(...,
                              aesthetics = "colour",
                              low = "#132B43",
                              high = "#56B1F7",
                              space = "Lab",
                              na.value = "grey50",
                              guide = "colourbar") {
  datetime_scale(
    aesthetics,
    "date",
    palette = seq_gradient_pal(low, high, space),
    na.value = na.value,
    guide = guide,
    ...
  )
}

ordinal_pal <- function(colours, na.color = "grey50", alpha = TRUE) {
  pal <- scales::colour_ramp(colours, na.color = na.color, alpha = alpha)
  function(n) {
    pal(seq(0, 1, length.out = n))
  }
}
