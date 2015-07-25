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

  required_aes = c(),

  calculate = function(self, data, scales, ...) {},

  calculate_groups = function(self, data, scales, ...) {
    if (empty(data)) return(data.frame())

    force(data)
    force(scales)

    # # Alternative approach: cleaner, but much slower
    # # Compute statistic for each group
    # stats <- ddply(data, "group", function(group) {
    #   self$calculate(group, scales, ...)
    # })
    # stats$ORDER <- seq_len(nrow(stats))
    #
    # # Combine statistics with original columns
    # unique <- ddply(data, .(group), uniquecols)
    # stats <- merge(stats, unique, by = "group")
    # stats[stats$ORDER, ]

    groups <- split(data, data$group)
    stats <- lapply(groups, function(group)
      self$calculate(data = group, scales = scales, ...))

    stats <- mapply(function(new, old) {
      if (empty(new)) return(data.frame())
      unique <- uniquecols(old)
      missing <- !(names(unique) %in% names(new))
      cbind(
        new,
        unique[rep(1, nrow(new)), missing,drop=FALSE]
      )
    }, stats, groups, SIMPLIFY=FALSE)

    do.call(plyr::rbind.fill, stats)
  }
)

# make_stat("bin") returns StatBin
make_stat <- function(class) {
  name <- paste0("Stat", camelize(class, first = TRUE))
  if (!exists(name)) {
    stop("No stat called ", name, ".", call. = FALSE)
  }

  obj <- get(name)
  if (!inherits(obj, "Stat")) {
    stop("Found object is not a stat", call. = FALSE)
  }

  obj
}
