


guide_custom <- function(
  grob, width = grobWidth(grob), height = grobHeight(grob),
  title = waiver(), title.position = "top",
  position = waiver(), order = 0
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
    title.position = title.position,
    hash = hash(list(title, grob)), # hash is already known
    position = position,
    order = order,
    available_aes = "any",
    super = GuideCustom
  )
}

GuideCustom <- ggproto(
  "GuideCustom", Guide,

  params = c(Guide$params, list(
    grob = NULL, width = NULL, height = NULL,
    title = waiver(),
    title.position = "top"
  )),

  hashables = exprs(title, grob),

  elements = list(
    background  = "legend.background",
    margin      = "legend.margin",
    theme.title = "legend.title"
  ),

  train = function(...) {
    params
  },

  transform = function(...) {
    params
  },

  override_elements = function(params, elements, theme) {
    elements$title <- elements$theme.title
    elements
  },

  draw = function(self, theme, params = self$params) {

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

    width  <- convertWidth(params$width, "cm")
    height <- convertHeight(params$height, "cm")
    gt <- gtable(widths = width, heights = height)
    gt <- gtable_add_grob(gt, params$grob, t = 1, l = 1)

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
