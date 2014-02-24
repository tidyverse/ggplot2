sd_section("Geoms",
  "Geoms, short for geometric objects, describe the type of plot you will produce.",
  c(
    "geom_abline",
    "geom_area",
    "geom_bar",
    "geom_bin2d",
    "geom_blank",
    "geom_boxplot",
    "geom_contour",
    "geom_crossbar",
    "geom_density",
    "geom_density2d",
    "geom_dotplot",
    "geom_errorbar",
    "geom_errorbarh",
    "geom_freqpoly",
    "geom_hex",
    "geom_histogram",
    "geom_hline",
    "geom_jitter",
    "geom_line",
    "geom_linerange",
    "geom_map",
    "geom_path",
    "geom_point",
    "geom_pointrange",
    "geom_polygon",
    "geom_quantile",
    "geom_raster",
    "geom_rect",
    "geom_ribbon",
    "geom_rug",
    "geom_segment",
    "geom_smooth",
    "geom_step",
    "geom_text",
    "geom_tile",
    "geom_violin",
    "geom_vline"
  )
)

sd_section("Statistics",
  "It's often useful to transform your data before plotting, and that's what statistical transformations do.",
  c(
    "stat_bin",
    "stat_bin2d",
    "stat_bindot",
    "stat_binhex",
    "stat_boxplot",
    "stat_contour",
    "stat_density",
    "stat_density2d",
    "stat_ecdf",
    "stat_function",
    "stat_identity",
    "stat_qq",
    "stat_quantile",
    "stat_smooth",
    "stat_spoke",
    "stat_sum",
    "stat_summary",
    "stat_summary_hex",
    "stat_summary2d",
    "stat_unique",
    "stat_ydensity"
  )
)

sd_section("Scales",
  "Scales control the mapping between data and aesthetics.",
  c(
    "expand_limits",
    "guides",
    "guide_legend",
    "guide_colourbar",
    "scale_alpha",
    "scale_area",
    "scale_size_area",
    "scale_colour_brewer",
    "scale_colour_gradient",
    "scale_colour_gradient2",
    "scale_colour_gradientn",
    "scale_colour_grey",
    "scale_colour_hue",
    "scale_identity",
    "scale_manual",
    "scale_linetype",
    "scale_shape",
    "scale_size",
    "scale_x_continuous",
    "scale_x_date",
    "scale_x_datetime",
    "scale_x_discrete",
    "labs",
    "update_labels",
    "xlim"
  )
)

sd_section("Coordinate systems",
  "Coordinate systems adjust the mapping from coordinates to the 2d plane of the computer screen.",
  c(
    "coord_cartesian",
    "coord_fixed",
    "coord_flip",
    "coord_map",
    "coord_polar",
    "coord_trans"
  )
)

sd_section("Faceting",
  "Facets display subsets of the dataset in different panels.",
  c(
    "facet_grid",
    "facet_null",
    "facet_wrap",
    "label_both",
    "label_bquote",
    "label_parsed",
    "label_value"
  )
)

sd_section("Position adjustments",
  "Position adjustments can be used to fine tune positioning of objects to achieve effects like dodging, jittering and stacking.",
  c(
    "position_dodge",
    "position_fill",
    "position_identity",
    "position_stack",
    "position_jitter"
  )
)

sd_section("Data",
  "Data sets included in ggplot2 and used in examples",
  c(
    "diamonds",
    "economics",
    "midwest",
    "movies",
    "mpg",
    "msleep",
    "presidential",
    "seals"
  )
)

sd_section("Anotation",
  "Specialised functions for adding annotations to a plot",
  c(
    "annotate",
    "annotation_custom",
    "annotation_logticks",
    "annotation_map",
    "annotation_raster",
    "borders"
  )
)

sd_section("Fortify",
  "Fortify methods make it possible to use ggplot2 with objects of
   various types, not just data frames.",
  c(
    "fortify",
    "fortify-multcomp",
    "fortify.lm",
    "fortify.map",
    "fortify.sp",
    "map_data"
  )
)

sd_section("Themes",
  "Themes control non-data components of the plot",
  c(
    "add_theme",
    "calc_element",
    "element_blank",
    "element_line",
    "element_rect",
    "element_text",
    "is.rel",
    "is.theme",
    "opts",
    "rel",
    "theme",
    "theme_blank",
    "theme_bw",
    "theme_classic",
    "theme_grey",
    "theme_minimal",
    "theme_update",
    "update_element"
  )
)

sd_section("Plot creation", "",
  c(
    "ggplot",
    "qplot",
    "+.gg",
    "autoplot",
    "ggplot.data.frame",
    "is.ggplot",
    "print.ggplot"
  )
)

sd_section("Aesthetics", "",
  c(
    "aes",
    "aes_all",
    "aes_auto",
    "aes_string",
    "aes_colour_fill_alpha",
    "aes_group_order",
    "aes_linetype_size_shape",
    "aes_position"
  )
)
