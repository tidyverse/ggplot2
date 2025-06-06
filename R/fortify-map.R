#' Fortify method for map objects
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function turns a map into a data frame that can more easily be
#' plotted with ggplot2.
#'
#' @export
#' @seealso [map_data()] and [annotation_borders()]
#' @param model map object
#' @param data not used by this method
#' @param ... not used by this method
#' @keywords internal
#' @examples
#' if (require("maps")) {
#' ca <- map("county", "ca", plot = FALSE, fill = TRUE)
#' head(fortify(ca))
#' ggplot(ca, aes(long, lat)) +
#'   geom_polygon(aes(group = group))
#' }
#'
#' if (require("maps")) {
#' tx <- map("county", "texas", plot = FALSE, fill = TRUE)
#' head(fortify(tx))
#' ggplot(tx, aes(long, lat)) +
#'   geom_polygon(aes(group = group), colour = "white")
#' }
fortify.map <- function(model, data, ...) {
  lifecycle::deprecate_warn(
    "3.6.0", I("`fortify(<map>)`"), "map_data()"
  )
  df <- data_frame0(
    long = model$x,
    lat = model$y,
    group = cumsum(is.na(model$x) & is.na(model$y)) + 1,
    order = seq_along(model$x),
    .size = length(model$x)
  )

  # TODO: convert to vec_rbind() once it accepts a function in .name_repair
  names <- lapply(strsplit(model$names, "[:,]"), "[", 1:2)
  names <- inject(rbind(!!!names))
  df$region <- names[df$group, 1]
  df$subregion <- names[df$group, 2]
  df[stats::complete.cases(df$lat, df$long), ]
}

#' Create a data frame of map data
#'
#' Easily turn data from the \pkg{maps} package into a data frame suitable
#' for plotting with ggplot2.
#'
#' @param map name of map provided by the \pkg{maps} package. These
#'   include [`"county"`][maps::county], [`"france"`][maps::france],
#'   [`"italy"`][maps::italy], [`"nz"`][maps::nz],
#'   [`"state"`][maps::state], [`"usa"`][maps::usa],
#'   [`"world"`][maps::world], or [`"world2"`][maps::world2].
#' @param region name(s) of subregion(s) to include. Defaults to `.` which
#'   includes all subregions. See documentation for [maps::map()]
#'   for more details.
#' @param exact should the `region` be treated as a regular expression
#'   (`FALSE`) or as a fixed string (`TRUE`).
#' @param ... all other arguments passed on to [maps::map()]
#' @keywords internal
#' @export
#' @examples
#' if (require("maps")) {
#' states <- map_data("state")
#' arrests <- USArrests
#' names(arrests) <- tolower(names(arrests))
#' arrests$region <- tolower(rownames(USArrests))
#'
#' choro <- merge(states, arrests, sort = FALSE, by = "region")
#' choro <- choro[order(choro$order), ]
#' ggplot(choro, aes(long, lat)) +
#'   geom_polygon(aes(group = group, fill = assault)) +
#'   coord_map("albers",  lat0 = 45.5, lat1 = 29.5)
#' }
#'
#' if (require("maps")) {
#' ggplot(choro, aes(long, lat)) +
#'   geom_polygon(aes(group = group, fill = assault / murder)) +
#'   coord_map("albers",  lat0 = 45.5, lat1 = 29.5)
#' }
map_data <- function(map, region = ".", exact = FALSE, ...) {
  check_installed("maps", reason = "for `map_data()`.")
  map_obj <- maps::map(map, region, exact = exact, plot = FALSE, fill = TRUE, ...)

  if (!inherits(map_obj, "map")) {
    cli::cli_abort(c(
      "{.fn maps::map} must return an object of type {.cls map}, not \\
      {obj_type_friendly(map_obj)}.",
      i = "Did you pass the right arguments?"
    ))
  }

  df <- data_frame0(
    long  = map_obj$x,
    lat   = map_obj$y,
    group = cumsum(is.na(map_obj$x) & is.na(map_obj$y)) + 1,
    order = seq_along(map_obj$x),
    .size = length(map_obj$x)
  )

  names <- lapply(strsplit(map_obj$names, "[:,]"), "[", 1:2)
  names <- vec_rbind(!!!names, .name_repair = ~ c("region", "subregion"))
  df[names(names)] <- vec_slice(names, df$group)
  vec_slice(df, stats::complete.cases(df$lat, df$long))
}
