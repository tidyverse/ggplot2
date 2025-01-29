
test_that("all keys can be drawn without 'params'", {

  params <- list()
  size <- convertUnit(calc_element("legend.key.size", theme_gray()), "cm", valueOnly = TRUE)
  size <- size * 10 # cm to mm

  # Render every key
  # If we're to develop new legend keys, we can keep appending this pattern
  # for new keys and layout should adjust automatically.
  # This is also an implicit test whether the key can be constructed without errors
  keys <- list(
    point      = draw_key_point(GeomPoint$use_defaults(NULL),           params, size),
    abline     = draw_key_abline(GeomAbline$use_defaults(NULL),         params, size),
    rect       = draw_key_rect(GeomRect$use_defaults(NULL),             params, size),
    polygon    = draw_key_polygon(GeomPolygon$use_defaults(NULL),       params, size),
    blank      = draw_key_blank(GeomBlank$use_defaults(NULL),           params, size),
    boxplot    = draw_key_boxplot(GeomBoxplot$use_defaults(NULL),       params, size),
    crossbar   = draw_key_crossbar(GeomCrossbar$use_defaults(NULL),     params, size),
    path       = draw_key_path(GeomPath$use_defaults(NULL),             params, size),
    vpath      = draw_key_vpath(GeomPath$use_defaults(NULL),            params, size),
    dotplot    = draw_key_dotplot(GeomDotplot$use_defaults(NULL),       params, size),
    linerange  = draw_key_linerange(GeomLinerange$use_defaults(NULL),   params, size),
    pointrange = draw_key_pointrange(GeomPointrange$use_defaults(NULL), params, size),
    smooth     = draw_key_smooth(GeomSmooth$use_defaults(NULL),         params, size),
    text       = draw_key_text(GeomText$use_defaults(NULL),             params, size),
    label      = draw_key_label(GeomLabel$use_defaults(NULL),           params, size),
    vline      = draw_key_vline(GeomVline$use_defaults(NULL),           params, size),
    timeseries = draw_key_timeseries(GeomPath$use_defaults(NULL),       params, size)
  )

  # Test that we've covered all exported keys above
  nse <- getNamespaceExports(asNamespace("ggplot2"))
  nse <- grep("^draw_key", nse, value = TRUE)
  nse <- gsub("^draw_key_", "", nse)
  expect_in(nse, names(keys))

  # Add title to every key
  template <- gtable(width = unit(size, "mm"), heights = unit(c(1, size), c("lines", "mm")))
  keys <- Map(
    function(key, name) {
      text <- textGrob(name, gp = gpar(fontsize = 8))
      gtable_add_grob(template, list(text, key), t = 1:2, l = 1, clip = "off")
    },
    key = keys, name = names(keys)
  )

  # Set layout
  n <- length(keys)
  nrow <- ceiling(n / 5)
  ncol <- ceiling(n / nrow)
  mtx <- matrix(list(zeroGrob()), nrow = nrow, ncol = ncol)
  mtx[seq_along(keys)] <- keys

  # Render as gtable
  gt <- gtable_matrix(
    name = "layout", grobs = mtx,
    widths  = unit(rep(size, ncol(mtx)), "mm"),
    heights = unit(rep(size, nrow(mtx)), "mm") + unit(1, "lines"),
    clip = "off"
  )
  gt <- gtable_add_col_space(gt, unit(1, "cm"))
  gt <- gtable_add_row_space(gt, unit(1, "cm"))

  expect_doppelganger("all legend keys", gt)
})
