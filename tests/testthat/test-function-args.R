filter_args <- function(x) {
  all_names <- names(x)
  all_names <- setdiff(all_names, c("self", "data", "scales", "coordinates", "..."))
  x[all_names]
}

find_partial_match_pairs <- function(args) {
  if (length(args) < 2) {
    return(NULL)
  }
  combinations <- combn(args, 2L)
  contains <- startsWith(combinations[1, ], combinations[2, ]) |
    startsWith(combinations[2, ], combinations[1, ])

  if (!any(contains)) {
    return(NULL)
  }

  problem <- combinations[, contains, drop = FALSE]
  paste0("`", problem[1, ], "` with `", problem[2, ], "`")
}


test_that("geom_xxx and GeomXxx$draw arg defaults match", {
  ggplot2_ns <- asNamespace("ggplot2")
  objs <- ls(ggplot2_ns)
  geom_fun_names <- objs[grepl("^(geom|annotation)_", objs)]
  # These aren't actually geoms, or need special parameters and can't be tested this way.
  geom_fun_names <- setdiff(
    geom_fun_names,
    c("geom_map", "geom_sf", "geom_smooth", "geom_column", "geom_area",
      "geom_density", "annotation_custom", "annotation_map", "annotation_raster",
      "annotation_id", "geom_errorbarh")
  )

  # For each geom_xxx function and the corresponding GeomXxx$draw and
  # GeomXxx$draw_groups functions, make sure that if they have same args, that
  # the args have the same default values.
  lapply(geom_fun_names, function(geom_fun_name) {
    geom_fun    <- ggplot2_ns[[geom_fun_name]]
    geom <- geom_fun()$geom
    if (!is_geom(geom)) # for geoms that return more than one thing
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
  # Remove deprecated stats
  stat_fun_names <- setdiff(stat_fun_names, c("stat_spoke", "stat_summary2d"))

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

# If the following tests fail, you may have introduced a potential partial match
# in argument names. The code should be double checked that is doesn't
# accidentally use `list$arg` when `list$arg_name` also exists. If that doesn't
# occur, the snapshot can be updated.

test_that("GeomXxx$parameters() does not contain partial matches", {
  ggplot2_ns <- asNamespace("ggplot2")
  objs <- ls(ggplot2_ns)
  geom_class_names <- grep("^Geom", objs, value = TRUE)
  geom_class_names <- setdiff(geom_class_names, c("Geom"))

  problems <- list()

  for (geom_class_name in geom_class_names) {
    geom_obj <- ggplot2_ns[[geom_class_name]]
    params <- geom_obj$parameters()
    issues <- find_partial_match_pairs(params)
    if (length(issues) == 0) {
      next
    }
    problems[[geom_class_name]] <- issues
  }

  problems <- vapply(problems, paste0, character(1), collapse = ", ")
  problems <- paste0(format(names(problems)), ": ", problems)
  expect_snapshot(problems)
})

test_that("StatXxx$parameters() does not contain partial matches", {
  ggplot2_ns <- asNamespace("ggplot2")
  objs <- ls(ggplot2_ns)
  stat_class_names <- grep("^Stat", objs, value = TRUE)
  stat_class_names <- setdiff(stat_class_names, c("Stat"))

  problems <- list()

  for (stat_class_name in stat_class_names) {
    stat_obj <- ggplot2_ns[[stat_class_name]]
    params <- stat_obj$parameters()
    issues <- find_partial_match_pairs(params)
    if (length(issues) == 0) {
      next
    }
    problems[[stat_class_name]] <- issues
  }
  problems <- vapply(problems, paste0, character(1), collapse = ", ")
  problems <- paste0(format(names(problems)), ": ", problems)
  expect_snapshot(problems)
})
