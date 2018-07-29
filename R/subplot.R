#' Create a ribbon or grid of subplots
#'
#' `subplot_wrap` and `subplot_grid` are variations of [facet_wrap()] and
#' [facet_grid()] that are more appropriate when the facets represent
#' different variables rather than different subsets of the same variable.
#'
#' @inheritParams facet_wrap
#' @inheritParams facet_grid
#' @param axis Axis to which the variables of the subplots are mapped.
#' @param scales Either "auto", in which case only the scale of the axis
#'   designated by `axis` is free, or "free" in which case both scales are
#'   free.
#' @param ... Passed to [facet_wrap()] or [facet_grid()].
#' @rdname subplot
#' @export
#' @examples
#' # Stack different variables in the same column
#' # NB: see tidyr::gather for a more practical alternative
#' cars <- stack(mtcars, select=c(disp, drat, qsec))
#' cars$mpg <- mtcars$mpg
#' cars$cyl <- mtcars$cyl
#' head(cars)
#'
#' # inspect the distribution of all stacked variables at once
#' p <- ggplot(cars) + geom_histogram(aes(values), bins=10)
#' p + facet_wrap(~ind, scales="free_x")
#' p + subplot_wrap(~ind)
#'
#' # examine the relationship of all variables with mpg
#' p <- ggplot(cars) + geom_point(aes(values, mpg))
#' p + facet_wrap(~ind, scales="free_x")
#' p + subplot_wrap(~ind)
#' # or in the other direction
#' p <- ggplot(cars) + geom_point(aes(mpg, values))
#' p + facet_wrap(~ind, scales="free_y")
#' p + subplot_wrap(~ind, axis="y")
#'
#' # do the same but with an additional conditional variable, hence
#' # generating a grid of subplots
#' p <- ggplot(cars) + geom_point(aes(values, mpg))
#' p + facet_grid(cyl~ind, scales="free_x")
#' p + subplot_grid(cyl~ind)
#'
#' # the facet labels play the role of the axis title and respect its theme
#' ggplot(cars) + geom_point(aes(values, mpg)) + subplot_wrap(~ind)
#' theme_update(axis.title=element_text(colour="red"))
#' ggplot(cars) + geom_point(aes(values, mpg)) + subplot_wrap(~ind)
#' # but only when the plot if created *after* global theme updates
#' p <- ggplot(cars) + geom_point(aes(values, mpg)) + subplot_wrap(~ind)
#' theme_update(axis.title=element_text(colour="blue"))
#' p
#' p + theme(axis.title=element_text(colour="green"))
#' # here, in both cases, the facet labels stay blue, unfortunately
subplot_wrap <- function(facets, axis="x", scales="auto", ...) {
  axis <- match.arg(axis, c("x", "y"))
  list(
    facet_wrap(facets=facets,
      scales=subplot_scales(scales, axis),
      strip.position=ifelse(axis=="x", "bottom", "left"), ...),
    subplot_theme(axis)
  )
}

#' @rdname subplot
#' @export
subplot_grid <- function(rows=NULL, cols=NULL, axis="x", scales="auto", ...) {
  axis <- match.arg(axis, c("x", "y"))
  list(
    facet_grid(rows=rows, cols=cols,
      scales=subplot_scales(scales, axis), switch=axis, ...),
    subplot_theme(axis)
  )
}

subplot_scales <- function(scales, axis) {
  scales <- match.arg(scales, c("auto", "free"))
  # with facets representing different variables, the scale of the axis
  # mapped to the variables should be free
  if (scales == "auto") {
    scales <- paste0("free_", axis)
  }
  return(scales)
}

subplot_theme <- function(axis) {
  if (axis == "x") {
    th <- theme(
      # give the strip text the appearance of the x axis title
      strip.background.x=element_blank(),
      strip.text.x=calc_element("axis.title.x", theme_get()),
      # TODO this gets the theme elements at the time of plot creation, rather than rendering, which means only global theme changes before the creation of the plot are respected. Ideally strip.text.* should inherit whatever is in axis.title.* at the time of rendering. Not sure if/how this is possible
      # remove the x axis title
      axis.title.x=element_blank()
    )
  } else {
    # do the same but for the y axis
    th <- theme(
      strip.background.y=element_blank(),
      strip.text.y=calc_element("axis.title.y", theme_get()),
      axis.title.y=element_blank()
    )
    # and rotate the text the correct way
    th$strip.text.y$angle <- -90
  }
  # put the facet strips where the axis titles would be
  th + theme(
    strip.placement="outside",
    strip.switch.pad.wrap=unit(0, "npc"),
    strip.switch.pad.grid=unit(0, "npc")
  )
}
