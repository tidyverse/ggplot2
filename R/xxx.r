coord_cartesian <- CoordCartesian$new
coord_equal <- CoordEqual$new
coord_flip <- CoordFlipped$new
coord_map <- CoordMap$new
coord_polar <- CoordPolar$new
coord_trans <- CoordTransform$new
facet_grid <- FacetGrid$new
geom_abline <- GeomAbline$new
geom_area <- GeomArea$new
geom_bar <- GeomBar$new
geom_blank <- GeomBlank$new
geom_boxplot <- GeomBoxplot$new
geom_contour <- GeomContour$new
geom_crossbar <- GeomCrossbar$new
geom_density <- GeomDensity$new
geom_density_2d <- GeomDensity2d$new
geom_errorbar <- GeomErrorbar$new
geom_histogram <- GeomHistogram$new
geom_hline <- GeomHline$new
geom_interval <- GeomInterval$new
geom_jitter <- GeomJitter$new
geom_line <- GeomLine$new
geom_linerange <- GeomLinerange$new
geom_path <- GeomPath$new
geom_point <- GeomPoint$new
geom_pointrange <- GeomPointrange$new
geom_polygon <- GeomPolygon$new
geom_quantile <- GeomQuantile$new
geom_ribbon <- GeomRibbon$new
geom_segment <- GeomSegment$new
geom_smooth <- GeomSmooth$new
geom_step <- GeomStep$new
geom_text <- GeomText$new
geom_tile <- GeomTile$new
geom_vline <- GeomVline$new
position_dodge <- PositionDodge$new
position_fill <- PositionFill$new
position_identity <- PositionIdentity$new
position_jitter <- PositionJitter$new
position_stack <- PositionStack$new
scale_area <- ScaleArea$new
scale_colour_brewer <- function(...) ScaleBrewer$new(..., variable = "colour")
scale_colour_colour <- function(...) ScaleColour$new(..., variable = "colour")
scale_colour_continuous <- function(...) ScaleColourContinuous$new(..., variable = "colour")
scale_colour_discrete <- function(...) ScaleColourDiscrete$new(..., variable = "colour")
scale_colour_gradient <- function(...) ScaleGradient$new(..., variable = "colour")
scale_colour_gradient2 <- function(...) ScaleGradient2$new(..., variable = "colour")
scale_colour_hue <- function(...) ScaleHue$new(..., variable = "colour")
scale_colour_identity <- function(...) ScaleIdentity$new(..., variable = "colour")
scale_colour_manual <- function(...) ScaleManual$new(..., variable = "colour")
scale_fill_brewer <- function(...) ScaleBrewer$new(..., variable = "fill")
scale_fill_colour <- function(...) ScaleColour$new(..., variable = "fill")
scale_fill_continuous <- function(...) ScaleColourContinuous$new(..., variable = "fill")
scale_fill_discrete <- function(...) ScaleColourDiscrete$new(..., variable = "fill")
scale_fill_gradient <- function(...) ScaleGradient$new(..., variable = "fill")
scale_fill_gradient2 <- function(...) ScaleGradient2$new(..., variable = "fill")
scale_fill_hue <- function(...) ScaleHue$new(..., variable = "fill")
scale_fill_identity <- function(...) ScaleIdentity$new(..., variable = "fill")
scale_fill_manual <- function(...) ScaleManual$new(..., variable = "fill")
scale_linetype <- ScaleLinetype$new
scale_linetype_identity <- function(...) ScaleIdentity$new(..., variable = "linetype")
scale_linetype_manual <- function(...) ScaleManual$new(..., variable = "linetype")
scale_shape <- ScaleShape$new
scale_shape_identity <- function(...) ScaleIdentity$new(..., variable = "shape")
scale_shape_manual <- function(...) ScaleManual$new(..., variable = "shape")
scale_size <- ScaleSize$new
scale_size_discrete <- ScaleSizeDiscrete$new
scale_size_identity <- function(...) ScaleIdentity$new(..., variable = "size")
scale_size_manual <- function(...) ScaleManual$new(..., variable = "size")
scale_x_asn <- function(...) ScaleAsn$new(..., variable = "x")
scale_x_atanh <- function(...) ScaleAtanh$new(..., variable = "x")
scale_x_continuous <- function(...) ScaleContinuous$new(..., variable = "x")
scale_x_date <- function(...) ScaleDate$new(..., variable = "x")
scale_x_discrete <- function(...) ScaleDiscrete$new(..., variable = "x")
scale_x_exp <- function(...) ScaleExp$new(..., variable = "x")
scale_x_inverse <- function(...) ScaleInverse$new(..., variable = "x")
scale_x_log <- function(...) ScaleLog$new(..., variable = "x")
scale_x_log10 <- function(...) ScaleLog10$new(..., variable = "x")
scale_x_log2 <- function(...) ScaleLog2$new(..., variable = "x")
scale_x_logit <- function(...) ScaleLogit$new(..., variable = "x")
scale_x_pow <- function(...) ScalePower$new(..., variable = "x")
scale_x_pow10 <- function(...) ScalePow10$new(..., variable = "x")
scale_x_prob <- function(...) ScaleProbability$new(..., variable = "x")
scale_x_probit <- function(...) ScaleProbit$new(..., variable = "x")
scale_x_reverse <- function(...) ScaleReverse$new(..., variable = "x")
scale_x_sqrt <- function(...) ScaleSqrt$new(..., variable = "x")
scale_xend_asn <- function(...) ScaleAsn$new(..., variable = "xend")
scale_xend_atanh <- function(...) ScaleAtanh$new(..., variable = "xend")
scale_xend_continuous <- function(...) ScaleContinuous$new(..., variable = "xend")
scale_xend_exp <- function(...) ScaleExp$new(..., variable = "xend")
scale_xend_inverse <- function(...) ScaleInverse$new(..., variable = "xend")
scale_xend_log <- function(...) ScaleLog$new(..., variable = "xend")
scale_xend_log10 <- function(...) ScaleLog10$new(..., variable = "xend")
scale_xend_log2 <- function(...) ScaleLog2$new(..., variable = "xend")
scale_xend_logit <- function(...) ScaleLogit$new(..., variable = "xend")
scale_xend_pow <- function(...) ScalePower$new(..., variable = "xend")
scale_xend_pow10 <- function(...) ScalePow10$new(..., variable = "xend")
scale_xend_prob <- function(...) ScaleProbability$new(..., variable = "xend")
scale_xend_probit <- function(...) ScaleProbit$new(..., variable = "xend")
scale_xend_reverse <- function(...) ScaleReverse$new(..., variable = "xend")
scale_xend_sqrt <- function(...) ScaleSqrt$new(..., variable = "xend")
scale_y_asn <- function(...) ScaleAsn$new(..., variable = "y")
scale_y_atanh <- function(...) ScaleAtanh$new(..., variable = "y")
scale_y_continuous <- function(...) ScaleContinuous$new(..., variable = "y")
scale_y_date <- function(...) ScaleDate$new(..., variable = "y")
scale_y_discrete <- function(...) ScaleDiscrete$new(..., variable = "y")
scale_y_exp <- function(...) ScaleExp$new(..., variable = "y")
scale_y_inverse <- function(...) ScaleInverse$new(..., variable = "y")
scale_y_log <- function(...) ScaleLog$new(..., variable = "y")
scale_y_log10 <- function(...) ScaleLog10$new(..., variable = "y")
scale_y_log2 <- function(...) ScaleLog2$new(..., variable = "y")
scale_y_logit <- function(...) ScaleLogit$new(..., variable = "y")
scale_y_pow <- function(...) ScalePower$new(..., variable = "y")
scale_y_pow10 <- function(...) ScalePow10$new(..., variable = "y")
scale_y_prob <- function(...) ScaleProbability$new(..., variable = "y")
scale_y_probit <- function(...) ScaleProbit$new(..., variable = "y")
scale_y_reverse <- function(...) ScaleReverse$new(..., variable = "y")
scale_y_sqrt <- function(...) ScaleSqrt$new(..., variable = "y")
scale_yend_asn <- function(...) ScaleAsn$new(..., variable = "yend")
scale_yend_atanh <- function(...) ScaleAtanh$new(..., variable = "yend")
scale_yend_continuous <- function(...) ScaleContinuous$new(..., variable = "yend")
scale_yend_exp <- function(...) ScaleExp$new(..., variable = "yend")
scale_yend_inverse <- function(...) ScaleInverse$new(..., variable = "yend")
scale_yend_log <- function(...) ScaleLog$new(..., variable = "yend")
scale_yend_log10 <- function(...) ScaleLog10$new(..., variable = "yend")
scale_yend_log2 <- function(...) ScaleLog2$new(..., variable = "yend")
scale_yend_logit <- function(...) ScaleLogit$new(..., variable = "yend")
scale_yend_pow <- function(...) ScalePower$new(..., variable = "yend")
scale_yend_pow10 <- function(...) ScalePow10$new(..., variable = "yend")
scale_yend_prob <- function(...) ScaleProbability$new(..., variable = "yend")
scale_yend_probit <- function(...) ScaleProbit$new(..., variable = "yend")
scale_yend_reverse <- function(...) ScaleReverse$new(..., variable = "yend")
scale_yend_sqrt <- function(...) ScaleSqrt$new(..., variable = "yend")
scale_z_asn <- function(...) ScaleAsn$new(..., variable = "z")
scale_z_atanh <- function(...) ScaleAtanh$new(..., variable = "z")
scale_z_continuous <- function(...) ScaleContinuous$new(..., variable = "z")
scale_z_discrete <- function(...) ScaleDiscrete$new(..., variable = "z")
scale_z_exp <- function(...) ScaleExp$new(..., variable = "z")
scale_z_inverse <- function(...) ScaleInverse$new(..., variable = "z")
scale_z_log <- function(...) ScaleLog$new(..., variable = "z")
scale_z_log10 <- function(...) ScaleLog10$new(..., variable = "z")
scale_z_log2 <- function(...) ScaleLog2$new(..., variable = "z")
scale_z_logit <- function(...) ScaleLogit$new(..., variable = "z")
scale_z_pow <- function(...) ScalePower$new(..., variable = "z")
scale_z_pow10 <- function(...) ScalePow10$new(..., variable = "z")
scale_z_prob <- function(...) ScaleProbability$new(..., variable = "z")
scale_z_probit <- function(...) ScaleProbit$new(..., variable = "z")
scale_z_reverse <- function(...) ScaleReverse$new(..., variable = "z")
scale_z_sqrt <- function(...) ScaleSqrt$new(..., variable = "z")
stat_bin <- StatBin$new
stat_boxplot <- StatBoxplot$new
stat_contour <- StatContour$new
stat_density <- StatDensity$new
stat_density_2d <- StatDensity2d$new
stat_identity <- StatIdentity$new
stat_qq <- StatQq$new
stat_quantile <- StatQuantile$new
stat_smooth <- StatSmooth$new
stat_sort <- StatSort$new
stat_sort_angle <- StatSortAngle$new
stat_step <- StatStep$new
stat_sum <- StatSum$new
stat_summary <- StatSummary$new
stat_unique <- StatUnique$new
