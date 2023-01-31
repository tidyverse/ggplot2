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
#'     `compute_layer()` is called once per layer, `compute_panel()`
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

    # Record columns that are not constant within groups. We will drop them later.
    non_constant_columns <- character(0)

    stats <- mapply(function(new, old) {
      # In this function,
      #
      #   - `new` is the computed result. All the variables will be picked.
      #   - `old` is the original data. There are 3 types of variables:
      #     1) If the variable is already included in `new`, it's ignored
      #        because the values of `new` will be used.
      #     2) If the variable is not included in `new` and the value is
      #        constant within the group, it will be picked.
      #     3) If the variable is not included in `new` and the value is not
      #        constant within the group, it will be dropped. We need to record
      #        the dropped columns to drop it consistently later.

      if (empty(new)) return(data_frame0())

      # First, filter out the columns already included `new` (type 1).
      old <- old[, !(names(old) %in% names(new)), drop = FALSE]

      # Then, check whether the rest of the columns have constant values (type 2)
      # or not (type 3).
      non_constant <- vapply(old, function(x) length(unique0(x)) > 1, logical(1L))

      # Record the non-constant columns.
      non_constant_columns <<- c(non_constant_columns, names(old)[non_constant])

      vec_cbind(
        new,
        # Note that, while the non-constant columns should be dropped, we don't
        # do this here because it can be filled by vec_rbind() later if either
        # one of the group has a constant value (see #4394 for the details).
        old[rep(1, nrow(new)), , drop = FALSE]
      )
    }, stats, groups, SIMPLIFY = FALSE)

    non_constant_columns <- unique0(non_constant_columns)

    # We are going to drop columns that are not constant within groups and not
    # carried over/recreated by the stat. This can produce unexpected results,
    # and hence we warn about it (variables in dropped_aes are expected so
    # ignored here).
    dropped <- non_constant_columns[!non_constant_columns %in% self$dropped_aes]
    if (length(dropped) > 0) {
      cli::cli_warn(c(
        "The following aesthetics were dropped during statistical transformation: {.field {glue_collapse(dropped, sep = ', ')}}",
        "i" = "This can happen when ggplot fails to infer the correct grouping structure in the data.",
        "i" = "Did you forget to specify a {.code group} aesthetic or to convert a numerical variable into a factor?"
      ))
    }

    # Finally, combine the results and drop columns that are not constant.
    data_new <- vec_rbind0(!!!stats)
    data_new[, !names(data_new) %in% non_constant_columns, drop = FALSE]
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
