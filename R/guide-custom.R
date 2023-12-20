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
  title = NULL, theme = NULL,
  position = NULL, order = 0
) {
  check_object(grob, is.grob, "a {.cls grob} object")
  check_object(width, is.unit, "a {.cls unit} object")
  check_object(height, is.unit, "a {.cls unit} object")
  if (length(width) != 1) {
    cli::cli_abort("{.arg width} must be a single {.cls unit}, not a unit vector.")
  }
  if (length(height) != 1) {
    cli::cli_abort("{.arg height} must be a single {.cls unit}, not a unit vector.")
  }

  new_guide(
    grob = grob,
    width = width,
    height = height,
    title = title,
    theme = theme,
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

  params = c(Guide$params, list(grob = NULL, width = NULL, height = NULL)),

  hashables = exprs(title, grob),

  elements = list(
    background = "legend.background",
    margin     = "legend.margin",
    title      = "legend.title",
    title_position = "legend.title.position"
  ),

  train = function(...) {
    params
  },

  transform = function(...) {
    params
  },

  setup_elements = function(params, elements, theme) {
    theme <- add_theme(theme, params$theme)
    title_position <- theme$legend.title.position %||% switch(
      params$direction, vertical = "top", horizontal = "left"
    )
    title_position <- arg_match0(
      title_position, .trbl, arg_nm = "legend.title.position"
    )
    theme$legend.title.position <- title_position
    theme$legend.key.spacing <- theme$legend.key.spacing %||% unit(5.5, "pt")
    gap <- calc_element("legend.key.spacing", theme)

    margin <- calc_element("text", theme)$margin
    title <- theme(text = element_text(
      hjust = 0, vjust = 0.5,
      margin = position_margin(title_position, margin, gap)
    ))
    elements$title <- calc_element("legend.title", add_theme(theme, title))
    Guide$setup_elements(params, elements, theme)
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {

    # Render title
    params$direction <- params$direction %||% direction
    elems <- self$setup_elements(params, self$elements, theme)
    elems <- self$override_elements(params, elems, theme)

    # Start with putting the main grob in a gtable
    width  <- convertWidth(params$width, "cm", valueOnly = TRUE)
    height <- convertHeight(params$height, "cm", valueOnly = TRUE)
    gt <- gtable(widths = unit(width, "cm"), heights = unit(height, "cm"))
    gt <- gtable_add_grob(gt, params$grob, t = 1, l = 1, clip = "off")

    # Render title
    if (!is.waive(params$title) && !is.null(params$title)) {
      title <- self$build_title(params$title, elems, params)
    } else {
      title <- zeroGrob()
    }

    # Add title
    if (!is.zero(title)) {
      common_args <- list(name = "title", clip = "off", grobs = title)
      if (elems$title_position == "top") {
        gt <- gtable_add_rows(gt, unit(height_cm(title), "cm"), pos = 0)
        gt <- inject(gtable_add_grob(gt, t = 1, l = 1, !!!common_args))
      } else if (elems$title_position == "bottom") {
        gt <- gtable_add_rows(gt, unit(height_cm(title), "cm"), pos = -1)
        gt <- inject(gtable_add_grob(gt, t = -1, l = 1, !!!common_args))
      } else if (elems$title_position == "left") {
        gt <- gtable_add_cols(gt, unit(width_cm(title), "cm"), pos = 0)
        gt <- inject(gtable_add_grob(gt, t = 1, l = 1, !!!common_args))
      } else if (elems$title_position == "right") {
        gt <- gtable_add_cols(gt, unit(width_cm(title), "cm"), pos = -1)
        gt <- inject(gtable_add_grob(gt, t = 1, l = -1, !!!common_args))
      }

      # Add extra space for large titles
      extra_width  <- max(0, width_cm(title) - width)
      extra_height <- max(0, height_cm(title) - height)
      just <- with(elems$title, rotate_just(angle, hjust, vjust))
      hjust <- just$hjust
      vjust <- just$vjust
      if (elems$title_position %in% c("top", "bottom")) {
        gt <- gtable_add_cols(gt, unit(extra_width * hjust, "cm"), pos = 0)
        gt <- gtable_add_cols(gt, unit(extra_width * (1 - hjust), "cm"), pos = -1)
      } else {
        gt <- gtable_add_rows(gt, unit(extra_height * (1 - vjust), "cm"), pos = 0)
        gt <- gtable_add_rows(gt, unit(extra_height * vjust, "cm"), pos = -1)
      }
    }

    # Add padding and background
    gt <- gtable_add_padding(gt, elems$margin)
    background <- element_grob(elems$background)
    gt <- gtable_add_grob(
      gt, background,
      t = 1, l = 1, r = -1, b = -1,
      z = -Inf, clip = "off"
    )

    gt
  }
)
