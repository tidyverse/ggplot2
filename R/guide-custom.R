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
    title = NULL,
    title.position = "top"
  )),

  hashables = exprs(title, grob),

  elements = list(
    background   = "legend.background",
    theme.margin = "legend.margin",
    theme.title  = "legend.title"
  ),

  train = function(...) {
    params
  },

  transform = function(...) {
    params
  },

  override_elements = function(params, elements, theme) {
    elements$title <- elements$theme.title
    elements$margin <- params$margin %||% elements$theme.margin
    elements
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {

    # Render title
    elems <- self$setup_elements(params, self$elements, theme)
    elems <- self$override_elements(params, elems, theme)
    if (!is.waive(params$title) && !is.null(params$title)) {
      title <- self$build_title(params$title, elems, params)
    } else {
      title <- zeroGrob()
    }
    title.position <- params$title.position
    if (is.zero(title)) {
      title.position <- "none"
    }

    width  <- convertWidth(params$width, "cm", valueOnly = TRUE)
    height <- convertHeight(params$height, "cm", valueOnly = TRUE)
    gt <- gtable(widths = unit(width, "cm"), heights = unit(height, "cm"))
    gt <- gtable_add_grob(gt, params$grob, t = 1, l = 1, clip = "off")

    extra_width  <- max(0, width_cm(title) - width)
    extra_height <- max(0, height_cm(title) - height)
    just <- with(elems$title, rotate_just(angle, hjust, vjust))
    hjust <- just$hjust
    vjust <- just$vjust

    if (params$title.position == "top") {
      gt <- gtable_add_rows(gt, elems$margin[1], pos = 0)
      gt <- gtable_add_rows(gt, unit(height_cm(title), "cm"), pos = 0)
      gt <- gtable_add_grob(gt, title, t = 1, l = 1, name = "title", clip = "off")
    } else if (params$title.position == "bottom") {
      gt <- gtable_add_rows(gt, elems$margin[3], pos = -1)
      gt <- gtable_add_rows(gt, unit(height_cm(title), "cm"), pos = -1)
      gt <- gtable_add_grob(gt, title, t = -1, l = 1, name = "title", clip = "off")
    } else if (params$title.position == "left") {
      gt <- gtable_add_cols(gt, elems$margin[4], pos = 0)
      gt <- gtable_add_cols(gt, unit(width_cm(title), "cm"), pos = 0)
      gt <- gtable_add_grob(gt, title, t = 1, l = 1, name = "title", clip = "off")
    } else if (params$title.position == "right") {
      gt <- gtable_add_cols(gt, elems$margin[2], pos = -1)
      gt <- gtable_add_cols(gt, unit(width_cm(title), "cm"), pos = 0)
      gt <- gtable_add_grob(gt, title, t = 1, l = -1, name = "title", clip = "off")
    }
    if (params$title.position %in% c("top", "bottom")) {
      gt <- gtable_add_cols(gt, unit(extra_width * hjust, "cm"), pos = 0)
      gt <- gtable_add_cols(gt, unit(extra_width * (1 - hjust), "cm"), pos = -1)
    } else {
      gt <- gtable_add_rows(gt, unit(extra_height * (1 - vjust), "cm"), pos = 0)
      gt <- gtable_add_rows(gt, unit(extra_height * vjust, "cm"), pos = -1)
    }

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
