context("function-args")

filter_args <- function(x) {
  all_names <- names(x)
  all_names <- setdiff(all_names, c("self", "data", "scales", "coordinates", "..."))
  x[all_names]
}

test_that("geom_xxx and GeomXxx$draw arg defaults match", {
  ggplot2_ns <- asNamespace("ggplot2")
  objs <- ls(ggplot2_ns)
  geom_fun_names <- objs[grepl("^(geom|annotation)_", objs)]
  # These aren't actually geoms, or need special parameters and can't be tested this way.
  geom_fun_names <- setdiff(
    geom_fun_names,
    c("geom_map", "geom_sf", "geom_smooth", "geom_column", "annotation_custom", "annotation_map",
      "annotation_raster", "annotation_id")
  )

  # For each geom_xxx function and the corresponding GeomXxx$draw and
  # GeomXxx$draw_groups functions, make sure that if they have same args, that
  # the args have the same default values.
  lapply(geom_fun_names, function(geom_fun_name) {
    geom_fun    <- ggplot2_ns[[geom_fun_name]]
    geom <- geom_fun()$geom
    if (!inherits(geom, "Geom")) # for geoms that return more than one thing
      return()

    fun_args <- formals(geom_fun)
    draw_args <- c(
      ggproto_formals(geom$draw_layer),
      ggproto_formals(geom$draw_group)
    )
    draw_args <- filter_args(draw_args)

    common_names <- intersect(names(fun_args), names(draw_args))

    expect_identical(fun_args[common_names], draw_args[common_names],
      info = paste0("Mismatch between arg defaults for ", geom_fun_name,
        " and ", class(geom_fun()$geom)[1], "'s $draw and/or $draw_group functions.")
    )
  })
})

test_that("stat_xxx and StatXxx$compute_panel arg defaults match", {
  ggplot2_ns <- asNamespace("ggplot2")
  objs <- ls(ggplot2_ns)
  stat_fun_names <- objs[grepl("^stat_", objs)]
  # These aren't actually stats, or need special parameters and can't be tested this way.
  stat_fun_names <- setdiff(
    stat_fun_names,
    c("stat_function", "stat_sf")
  )

  # For each stat_xxx function and the corresponding StatXxx$compute_panel and
  # StatXxx$compute_group functions, make sure that if they have same args, that
  # the args have the same default values.
  lapply(stat_fun_names, function(stat_fun_name) {
    stat_fun         <- ggplot2_ns[[stat_fun_name]]
    calculate        <- stat_fun()$stat$compute_panel
    calculate_groups <- stat_fun()$stat$compute_group

    fun_args <- formals(stat_fun)
    calc_args <- c(ggproto_formals(calculate), ggproto_formals(calculate_groups))
    calc_args <- filter_args(calc_args)

    common_names <- intersect(names(fun_args), names(calc_args))

    expect_identical(fun_args[common_names], calc_args[common_names],
      info = paste0("Mismatch between arg defaults for ", stat_fun_name,
        " and ", class(stat_fun()$stat)[1], "'s $compute_panel and/or $compute_group functions.")
    )
  })
})
