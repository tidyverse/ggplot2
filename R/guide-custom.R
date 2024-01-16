#' Custom guides
#'
#' This is a special guide that can be used to display any graphical object
#' (grob) along with the regular guides. This guide has no associated scale.
#'
#' @param grob A grob to display.
#' @param width,height The allocated width and height to display the grob, given
#'  in [grid::unit()]s.
#' @param title A character string or expression indicating the title of guide.
#'   If `NULL` (default), no title is shown.
#' @param title.position A character string indicating the position of a title.
#'   One of `"top"` (default), `"bottom"`, `"left"` or `"right"`.
#' @param margin Margins around the guide. See [margin()] for more details. If
#'   `NULL` (default), margins are taken from the `legend.margin` theme setting.
#' @param position Currently not in use.
#' @inheritParams guide_legend
#'
#' @export
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#'
#' # Define a graphical object
#' circle <- grid::circleGrob()
#'
#' # Rendering a grob as a guide
#' p + guides(custom = guide_custom(circle, title = "My circle"))
#'
#' # Controlling the size of the grob defined in relative units
#' p + guides(custom = guide_custom(
#'   circle, title = "My circle",
#'   width = unit(2, "cm"), height = unit(2, "cm"))
#' )
#'
#' # Size of grobs in absolute units is taken directly without the need to
#' # set these manually
#' p + guides(custom = guide_custom(
#'   title = "My circle",
#'   grob = grid::circleGrob(r = unit(1, "cm"))
#' ))
guide_custom <- function(
  grob, width = grobWidth(grob), height = grobHeight(grob),
  title = NULL, title.position = "top", margin = NULL,
  position = NULL, order = 0
) {
  check_object(grob, is.grob, "a {.cls grob} object")
  check_object(width, is.unit, "a {.cls unit} object")
  check_object(height, is.unit, "a {.cls unit} object")
  check_object(margin, is.margin, "a {.cls margin} object", allow_null = TRUE)
  if (length(width) != 1) {
    cli::cli_abort("{.arg width} must be a single {.cls unit}, not a unit vector.")
  }
  if (length(height) != 1) {
    cli::cli_abort("{.arg height} must be a single {.cls unit}, not a unit vector.")
  }
  title.position <- arg_match0(title.position, .trbl)

  new_guide(
    grob = grob,
    width = width,
    height = height,
    title = title,
    title.position = title.position,
    margin = margin,
    hash = hash(list(title, grob)), # hash is already known
    position = position,
    order = order,
    available_aes = "any",
    super = GuideCustom
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GuideCustom <- ggproto(
  "GuideCustom", Guide,

  params = c(Guide$params, list(
    grob = NULL, width = NULL, height = NULL,
    margin = NULL,
    title.position = "top"
  )),

  hashables = exprs(title, grob),

  elements = list(
    background = "legend.background",
    margin     = "legend.margin",
    title      = "legend.title"
  ),

  train = function(...) {
    params
  },

  transform = function(...) {
    params
  },

  setup_elements = function(params, elements, theme) {
    theme <- add_theme(theme, params$theme)

    elements$title_position <- params$title.position %||%
      theme$legend.title.position %||%
      switch(params$idreciton, vertical = "top", horizontal = "left")
    margin <- position_margin(
      elements$title_position,
      calc_element("text", theme)$margin,
      calc_element("legend.key.spacing", theme)
    )
    title_theme <-
      theme(text = element_text(hjust = 0, vjust = 0.5, margin = margin))
    elements$title <-
      calc_element("legend.title", add_theme(theme, title_theme))
    elements$background <-
      element_grob(calc_element("legend.background", theme))
    elements$margin <-
      calc_element("legend.margin", theme)
    elements
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {

    # Render title
    elems <- self$setup_elements(params, self$elements, theme)
    if (!is.waive(params$title) && !is.null(params$title)) {
      title <- self$build_title(params$title, elems, params)
    } else {
      title <- zeroGrob()
    }
    title.position <- elems$title.position

    width  <- convertWidth(params$width, "cm", valueOnly = TRUE)
    height <- convertHeight(params$height, "cm", valueOnly = TRUE)
    gt <- gtable(widths = unit(width, "cm"), heights = unit(height, "cm"))
    gt <- gtable_add_grob(gt, params$grob, t = 1, l = 1, clip = "off")

    gt <- legend_add_title(
      gt, title, title.position,
      with(elems$title, rotate_just(angle, hjust, vjust))
    )

    gt <- gtable_add_padding(gt, elems$margin)

    gt <- gtable_add_grob(
      gt, elems$backgroun,
      t = 1, l = 1, r = -1, b = -1,
      z = -Inf, clip = "off"
    )
    gt
  }
)
