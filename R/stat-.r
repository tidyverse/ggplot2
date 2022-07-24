#' @section Stats:
#'
#' All `stat_*()` functions (like `stat_bin()`) return a layer that
#' contains a `Stat*` object (like `StatBin`). The `Stat*`
#' object is responsible for rendering the data in the plot.
#'
#' Each of the `Stat*` objects is a [ggproto()] object, descended
#' from the top-level `Stat`, and each implements various methods and
#' fields. To create a new type of Stat object, you typically will want to
#' override one or more of the following:
#'
#'   - One of :
#'     `compute_layer(self, data, scales, ...)`,
#'     `compute_panel(self, data, scales, ...)`, or
#'     `compute_group(self, data, scales, ...)`.
#'
#'     `compute_layer()` is called once per layer, `compute_panel_()`
#'     is called once per panel, and `compute_group()` is called once per
#'     group. All must return a data frame.
#'
#'     It's usually best to start by overriding `compute_group`: if
#'     you find substantial performance optimisations, override higher up.
#'     You'll need to read the source code of the default methods to see
#'     what else you should be doing.
#'
#'     `data` is a data frame containing the variables named according
#'     to the aesthetics that they're mapped to. `scales` is a list
#'     containing the `x` and `y` scales. There functions are called
#'     before the facets are trained, so they are global scales, not local
#'     to the individual panels.`...` contains the parameters returned by
#'     `setup_params()`.
#'   - `finish_layer(data, params)`: called once for each layer. Used
#'     to modify the data after scales has been applied, but before the data is
#'     handed of to the geom for rendering. The default is to not modify the
#'     data. Use this hook if the stat needs access to the actual aesthetic
#'     values rather than the values that are mapped to the aesthetic.
#'   - `setup_params(data, params)`: called once for each layer.
#'     Used to setup defaults that need to complete dataset, and to inform
#'     the user of important choices. Should return list of parameters.
#'   - `setup_data(data, params)`: called once for each layer,
#'     after `setup_params()`. Should return modified `data`.
#'     Default methods removes all rows containing a missing value in
#'     required aesthetics (with a warning if `!na.rm`).
#'   - `required_aes`: A character vector of aesthetics needed to
#'     render the geom.
#'   - `default_aes`: A list (generated by [aes()] of
#'     default values for aesthetics.
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
Stat <- ggproto("Stat",
  # Should the values produced by the statistic also be transformed
  # in the second pass when recently added statistics are trained to
  # the scales
  retransform = TRUE,

  default_aes = aes(),

  required_aes = character(),

  non_missing_aes = character(),

  # Any aesthetics that are dropped from the data frame during the
  # statistical transformation should be listed here to suppress a
  # warning about dropped aesthetics
  dropped_aes = character(),

  optional_aes = character(),

  setup_params = function(data, params) {
    params
  },

  setup_data = function(data, params) {
    data
  },

  compute_layer = function(self, data, params, layout) {
    check_required_aesthetics(
      self$required_aes,
      c(names(data), names(params)),
      snake_class(self)
    )

    # Make sure required_aes consists of the used set of aesthetics in case of
    # "|" notation in self$required_aes
    required_aes <- intersect(
      names(data),
      unlist(strsplit(self$required_aes, "|", fixed = TRUE))
    )

    data <- remove_missing(data, params$na.rm,
      c(required_aes, self$non_missing_aes),
      snake_class(self),
      finite = TRUE
    )

    # Trim off extra parameters
    params <- params[intersect(names(params), self$parameters())]

    args <- c(list(data = quote(data), scales = quote(scales)), params)
    dapply(data, "PANEL", function(data) {
      scales <- layout$get_scales(data$PANEL[1])
      try_fetch(
        inject(self$compute_panel(data = data, scales = scales, !!!params)),
        error = function(cnd) {
          cli::cli_warn("Computation failed in {.fn {snake_class(self)}}", parent = cnd)
          data_frame0()
        }
      )
    })
  },

  compute_panel = function(self, data, scales, ...) {
    if (empty(data)) return(data_frame0())

    groups <- split(data, data$group)
    stats <- lapply(groups, function(group) {
      self$compute_group(data = group, scales = scales, ...)
    })

    # record dropped columns
    dropped_columns <- new_environment()
    stats <- mapply(function(new, old) {
      if (empty(new)) return(data_frame0())

      # ignore the columns that will be overwritten by `new`
      old <- old[, !(names(old) %in% names(new)), drop = FALSE]

      # drop columns that are not constant within group
      unique_idx <- vapply(old, function(x) length(unique0(x)) == 1, logical(1L))
      env_bind(dropped_columns, !!!set_names(names(old)[!unique_idx]))

      result <- vec_cbind(
        new,
        old[rep(1, nrow(new)), unique_idx, drop = FALSE]
      )
    }, stats, groups, SIMPLIFY = FALSE)

    data_new <- vec_rbind(!!!stats)

    # The above code will drop columns that are not constant within groups and not
    # carried over/recreated by the stat. This can produce unexpected results,
    # and hence we warn about it.
    dropped <- ls(dropped_columns)
    dropped <- dropped[!dropped %in% self$dropped_aes]
    if (length(dropped) > 0) {
      cli::cli_warn(c(
        "The following aesthetics were dropped during statistical transformation: {.field {glue_collapse(dropped, sep = ', ')}}",
        "i" = "This can happen when ggplot fails to infer the correct grouping structure in the data.",
        "i" = "Did you forget to specify a {.code group} aesthetic or to convert a numerical variable into a factor?"
      ))
    }
    data_new
  },

  compute_group = function(self, data, scales) {
    cli::cli_abort("Not implemented")
  },

  finish_layer = function(self, data, params) {
    data
  },


  # See discussion at Geom$parameters()
  extra_params = "na.rm",
  parameters = function(self, extra = FALSE) {
    # Look first in compute_panel. If it contains ... then look in compute_group
    panel_args <- names(ggproto_formals(self$compute_panel))
    group_args <- names(ggproto_formals(self$compute_group))
    args <- if ("..." %in% panel_args) group_args else panel_args

    # Remove arguments of defaults
    args <- setdiff(args, names(ggproto_formals(Stat$compute_group)))

    if (extra) {
      args <- union(args, self$extra_params)
    }
    args
  },

  aesthetics = function(self) {
    if (is.null(self$required_aes)) {
      required_aes <- NULL
    } else {
      required_aes <- unlist(strsplit(self$required_aes, '|', fixed = TRUE))
    }
    c(union(required_aes, names(self$default_aes)), self$optional_aes, "group")
  }

)
