#' Extract tick information from guides
#'
#' `guide_data()` builds a plot and extracts information from guide keys. This
#'   information typically contains positions, values and/or labels, depending
#'   on which aesthetic is queried or guide is used.
#'
#' @param plot A `ggplot` object, or `ggplot_build` object.
#' @param aesthetic A scalar character that describes an aesthetic for which
#'   to extract guide information. For example `"colour"`, `"size"`, `"x"` or
#'   `"y.sec"`.
#' @param i,j An integer giving a row (i) or column (j) number of a facet for
#'   which to return position guide information.
#'
#' @details When used with plots containing `coord_sf()`, an aesthetic of `"x"`
#'   is used for the bottom axis, `"x.sec"` for the top axis, `"y"` for the
#'   left axis and `"y.sec"` for the right axis. This is also the default for
#'   other coords, but this can be changed for other coords with the `guides()`
#'   function.
#'
#' @return A `data.frame` containing information extracted from the guide key,
#'   or `NULL` when no such information could be found.
#' @export
#'
#' @examples
#' # A typical plot
#' p <- ggplot(mtcars) +
#'   aes(mpg, disp, colour = drat, size = drat) +
#'   geom_point() +
#'   facet_wrap(vars(cyl), scales = "free_x")
#'
#' # Getting guide data for position aesthetic in particular panel
#' guide_data(p, "x", i = 1, j = 2)
#'
#' # Getting guide data for a legend
#' guide_data(plot, "size")
#'
#' # If guides are merged, `guide_data()` reports merged guide data
#' guide_data(plot + guides(colour = "legend"), "size")
guide_data <- function(
  plot = last_plot(), aesthetic, i = 1L, j = 1L
) {
  # For now, only handles single aesthetics
  if (!is_scalar_character(aesthetic)) {
    cli::cli_abort(paste0(
      "{.arg aesthetic} must be a {.cls character} of length 1."
    ))
  }

  # Pre-build plot
  if (!inherits(plot, "ggplot_built")) {
    plot <- ggplot_build(plot)
  }

  # Decide whether to get position or regular guide information
  if (aesthetic %in% c("x", "y", "x.sec", "y.sec", "theta", "r")) {
    ans <- guide_data_position(plot, aesthetic, i = i, j = j)
  } else {
    # Regular guides don't carry any panel-wise information, so i/j are dropped
    ans <- guide_data_legend(plot, aesthetic)
  }
  ans
}

guide_data_position <- function(plot, aesthetic, i = 1L, j = 1L) {

  # We might expect guides to be unnamed, but matchable by position
  k <- match(aesthetic, c("x", "y", "x.sec", "y.sec",
                          "theta", "theta.sec", "r", "r.sec"))

  # Select relevant panel
  layout   <- plot$layout$layout
  selected <- layout[layout$ROW == i & layout$COL == j, , drop = FALSE]
  if (nrow(selected) < 1) {
    cli::cli_warn(c(paste0(
      "Selection with {.arg i = {i[1]}} and {.arg j = {j[1]}} yielded no ",
      "suitable panels."
    ), "!" = "{.code NULL} was returned."))
    return(NULL)
  }

  # Execute `plot$layout$setup_panel_guides()` for relevant panels only
  panels <- lapply(
    plot$layout$panel_params[selected$PANEL],
    plot$layout$coord$setup_panel_guides,
    plot$plot$guides,
    plot$layout$coord_params
  )
  panels <- lapply(
    panels,
    plot$layout$coord$train_panel_guides,
    plot$plot$layers,
    plot$plot$mapping,
    plot$layout$coord_params
  )

  # For coord_polar(), translate x/y to theta/r
  if (!is.null(plot$layout$coord$theta)) {
    if (plot$layout$coord$theta == "y") {
      aesthetic <- switch(
        aesthetic,
        "x" = "r", "x.sec" = "r.sec",
        "y" = "theta", "y.sec" = "theta.sec",
        aesthetic
      )
    } else {
      aesthetic <- switch(
        aesthetic,
        "x" = "theta", "x.sec" = "theta.sec",
        "y" = "r", "y.sec" = "r.sec",
        aesthetic
      )
    }
  }

  # Loop through panel, extract guide information
  lapply(panels, function(panel) {
    if (!is_empty(panel$guides)) {
      # We have coords with proper guides
      if (is.null(names(panel$guides))) {
        panel$guides[[k]]$key
      } else {
        panel$guides[[aesthetic]]$key
      }
    } else if (!is_empty(panel$graticule)) {
      # We have CoordSf which has graticules
      position <- switch(
        aesthetic,
        "x" = "bottom", "x.sec" = "top",
        "y" = "left",   "y.sec" = "right",
        cli::cli_abort(paste0(
          "Cannot extract guide data from graticule for ",
          "{.arg aesthetic = '{aesthetic}'}."
        ))
      )
      graticule_to_ticks(panel$graticule, panel, position)
    } else {
      # We have coords without proper guides
      cols <- paste0(aesthetic, c(".major", ".labels"))
      out <- data_frame0(major  = panel[[cols[1]]], labels = panel[[cols[2]]])
      colnames(out) <- cols
      out
    }
  })
}

guide_data_legend <- function(plot, aesthetic, ...) {

  # Prepare theme information
  theme <- plot_theme(plot$plot)
  position <- theme$legend.position %||% "right"
  if (position == "none") {
    return(NULL)
  }

  # Pick all scales, since guides can be merged
  scales <- plot$plot$scales$non_position_scales()
  if (!scales$has_scale(aesthetic)) {
    cli::cli_warn(c(paste0(
      "No scale for {.arg aesthetic = '{aesthetic}'} was found."
    ), "!" = "{.code NULL} was returned."))
    return(NULL)
  }

  # Mimic `build_guides`
  theme$legend.direction <- switch(
    legend_position(position),
    "inside"     = "vertical",
    "vertical"   = "vertical",
    "horizontal" = "horizontal"
  )

  # Train guides to build keys from scales
  guides <- guides_train(
    scales = scales,
    theme  = theme,
    guides = plot$plot$guides,
    labels = plot$plot$mapping
  )
  if (length(guides) == 0) {
    return(NULL)
  }

  # Merge guides
  guides   <- guides_merge(guides)

  # Extract keys
  guides   <- lapply(guides, `[[`, "key")
  my_guide <- vapply(guides, function(x) aesthetic %in% colnames(x), logical(1))
  unname(guides)[my_guide]
}


