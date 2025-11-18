#' @rdname Stat
#' @format NULL
#' @usage NULL
#' @export
StatBin <- ggproto(
  "StatBin", Stat,
  setup_params = function(self, data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE)

    if (is.logical(params$drop)) {
      params$drop <- if (isTRUE(params$drop)) "all" else "none"
    }
    drop <- params$drop
    params$drop <- arg_match0(
      params$drop %||% "none",
      c("all", "none", "extremes"), arg_nm = "drop"
    )

    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      cli::cli_abort("{.fn {snake_class(self)}} requires an {.field x} or {.field y} aesthetic.")
    }
    if (has_x && has_y) {
      cli::cli_abort("{.fn {snake_class(self)}} must only have an {.field x} {.emph or} {.field y} aesthetic.")
    }

    x <- flipped_names(params$flipped_aes)$x
    if (is_mapped_discrete(data[[x]])) {
      cli::cli_abort(c(
        "{.fn {snake_class(self)}} requires a continuous {.field {x}} aesthetic.",
        "x" = "the {.field {x}} aesthetic is discrete.",
        "i" = "Perhaps you want {.code stat=\"count\"}?"
      ))
    }

    params <- fix_bin_params(params, fun = snake_class(self), version = "2.1.0")
    params
  },

  extra_params = c("na.rm", "orientation"),

  compute_group = function(data, scales, binwidth = NULL, bins = NULL,
                           center = NULL, boundary = NULL,
                           closed = c("right", "left"), pad = FALSE,
                           breaks = NULL, flipped_aes = FALSE, drop = "none") {
    x <- flipped_names(flipped_aes)$x
    bins <- compute_bins(
      data[[x]], scales[[x]],
      breaks = breaks, binwidth = binwidth, bins = bins,
      center = center, boundary = boundary, closed = closed
    )
    bins <- bin_vector(data[[x]], bins, weight = data$weight, pad = pad)

    keep <- switch(
      drop,
      all = bins$count != 0,
      extremes = inner_runs(bins$count != 0),
      TRUE
    )
    bins <- vec_slice(bins, keep)
    bins$flipped_aes <- flipped_aes
    flip_data(bins, flipped_aes)
  },

  compute_panel = function(self, data, scales, binwidth = NULL, bins = NULL,
                           center = NULL, boundary = NULL,
                           closed = c("right", "left"), pad = FALSE,
                           breaks = NULL, flipped_aes = FALSE, drop = "none") {
    # First call parent's compute_panel to get binned data for all groups
    data <- ggproto_parent(Stat, self)$compute_panel(
      data, scales, binwidth = binwidth, bins = bins,
      center = center, boundary = boundary, closed = closed,
      pad = pad, breaks = breaks, flipped_aes = flipped_aes, drop = drop
    )
    
    # Only calculate bin_prop if we have the necessary columns and multiple groups
    if (!is.null(data) && nrow(data) > 0 && 
        all(c("count", "xmin", "xmax") %in% names(data))) {
      
      # Calculate bin_prop: proportion of each group within each bin
      # Create a unique bin identifier using rounded values to handle floating point precision
      data$bin_id <- paste(round(data$xmin, 10), round(data$xmax, 10), sep = "_")
      
      # Calculate total count per bin across all groups
      bin_totals <- stats::aggregate(data$count, by = list(bin_id = data$bin_id), FUN = sum)
      names(bin_totals)[2] <- "bin_total"
      
      # Merge back to get bin totals for each row
      data <- merge(data, bin_totals, by = "bin_id", sort = FALSE)
      
      # Calculate bin_prop: count within group / total count in bin
      # When bin_total = 0 (empty bin), set bin_prop based on whether there are multiple groups
      n_groups <- length(unique(data$group))
      if (n_groups == 1) {
        # With only one group, bin_prop is always 1 (100% of the bin belongs to this group)
        data$bin_prop <- 1
      } else {
        # With multiple groups, bin_prop = count / total_count_in_bin, or 0 for empty bins
        data$bin_prop <- ifelse(data$bin_total > 0, data$count / data$bin_total, 0)
      }
      
      # Remove the temporary columns
      data$bin_id <- NULL
      data$bin_total <- NULL
    } else {
      # If we don't have the necessary data, just add a default bin_prop column
      data$bin_prop <- if (nrow(data) > 0) rep(1, nrow(data)) else numeric(0)
    }
    
    data
  },

  default_aes = aes(x = after_stat(count), y = after_stat(count), weight = 1),

  required_aes = "x|y",

  dropped_aes = "weight" # after statistical transformation, weights are no longer available
)

#' @param binwidth The width of the bins. Can be specified as a numeric value
#'   or as a function that takes x after scale transformation as input and
#'   returns a single numeric value. When specifying a function along with a
#'   grouping structure, the function will be called once per group.
#'   The default is to use the number of bins in `bins`,
#'   covering the range of the data. You should always override
#'   this value, exploring multiple widths to find the best to illustrate the
#'   stories in your data.
#'
#'   The bin width of a date variable is the number of days in each time; the
#'   bin width of a time variable is the number of seconds.
#' @param bins Number of bins. Overridden by `binwidth`. Defaults to 30.
#' @param center,boundary bin position specifiers. Only one, `center` or
#'   `boundary`, may be specified for a single plot. `center` specifies the
#'   center of one of the bins. `boundary` specifies the boundary between two
#'   bins. Note that if either is above or below the range of the data, things
#'   will be shifted by the appropriate integer multiple of `binwidth`.
#'   For example, to center on integers use `binwidth = 1` and `center = 0`, even
#'   if `0` is outside the range of the data. Alternatively, this same alignment
#'   can be specified with `binwidth = 1` and `boundary = 0.5`, even if `0.5` is
#'   outside the range of the data.
#' @param breaks Alternatively, you can supply a numeric vector giving
#'    the bin boundaries. Overrides `binwidth`, `bins`, `center`,
#'    and `boundary`. Can also be a function that takes group-wise values as input and returns bin boundaries.
#' @param closed One of `"right"` or `"left"` indicating whether right
#'   or left edges of bins are included in the bin.
#' @param pad If `TRUE`, adds empty bins at either end of x. This ensures
#'   frequency polygons touch 0. Defaults to `FALSE`.
#' @param drop Treatment of zero count bins. If `"none"` (default), such
#'   bins are kept as-is. If `"all"`, all zero count bins are filtered out.
#'   If `"extremes"` only zero count bins at the flanks are filtered out, but
#'   not in the middle. `TRUE` is shorthand for `"all"` and `FALSE` is shorthand
#'   for `"none"`.
#' @eval rd_computed_vars(
#'   count    = "number of points in bin.",
#'   density  = "density of points in bin, scaled to integrate to 1.",
#'   ncount   = "count, scaled to a maximum of 1.",
#'   ndensity = "density, scaled to a maximum of 1.",
#'   width    = "widths of bins.",
#'   bin_prop = "proportion of points in bin that belong to each group."
#' )
#'
#' @section Dropped variables:
#' \describe{
#'   \item{`weight`}{After binning, weights of individual data points (if supplied) are no longer available.}
#' }
#'
#' @seealso [stat_count()], which counts the number of cases at each x
#'   position, without binning. It is suitable for both discrete and continuous
#'   x data, whereas `stat_bin()` is suitable only for continuous x data.
#' @export
#' @rdname geom_histogram
stat_bin <- make_constructor(
  StatBin, geom = "bar", position = "stack",
  orientation = NA
)

inner_runs <- function(x) {
  rle <- vec_unrep(x)
  nruns <- nrow(rle)
  inner <- rep(TRUE, nruns)
  i <- unique(c(1, nruns))
  inner[i] <- inner[i] & rle$key[i]
  rep(inner, rle$times)
}
