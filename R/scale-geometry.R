#' Geometry scale
#'
#' The geometry scale is a translation scale, transforming feature identifiers
#' into geometry using sf objects.
#'
#' @param data An sf object with a simple feature list (geometry) column.
#' @param id A string giving a column name in `data` to use as feature identifiers.
#' @param ... Passed on to [discrete_scale].
#'
#' @export
#' @include scale-identity.R
#'
#' @examplesIf require("rnaturalearth")
#' world <- rnaturalearth::ne_countries()
#'
#' # 1:1 mapping using three-letter country codes
#' iso_a3 <- data.frame(id = c("AUS", "NZL", "PNG", "IDN"))
#'
#' ggplot(iso_a3) +
#'   geom_sf(aes(geometry = id, fill = id)) +
#'   scale_geometry_discrete(world, "iso_a3")
#'
#' # 1:many mapping using regions
#' regions <- data.frame(id = c("Asia", "Europe"))
#'
#' ggplot(regions) +
#'   geom_sf(aes(geometry = id, fill = id)) +
#'   scale_geometry_discrete(world, "region_un")
#'
#' # Subnational identities
#' states <- rnaturalearth::ne_states(country = "united states of america")
#'
#' west_us <- data.frame(id = c("WA", "OR", "CA"))
#'
#' ggplot(west_us) +
#'   geom_sf(aes(geometry = id, fill = id)) +
#'   scale_geometry_discrete(states, "postal")
scale_geometry_discrete <- function(data = NULL, id = NULL, ...) {
  check_installed("sf")
  transform <- new_geometry_transform(data, id)
  scale <- discrete_scale(
    aesthetics = "geometry",
    ...,
    palette = pal_identity(),
    guide = "none",
    super = ScaleDiscreteGeometry
  )
  ggproto(NULL, scale, trans = transform)
}

ScaleDiscreteGeometry <- ggproto(
  "ScaleGeometry", ScaleDiscreteIdentity,
  transform = function(self, x) {
    self$trans(x)
  }
)

new_geometry_transform <- function(data, id, call = caller_call()) {
  force(call)
  check_inherits(data, "sf")
  check_string(id)
  columns <- colnames(data)
  if (!id %in% columns) {
    cli::cli_abort("{.arg id} must be a column in {.arg data}.", call = call)
  }
  geom_col <- geom_column(data)
  if (is.integer(geom_col)) {
    geom_col <- columns[geom_col]
  }
  if (!geom_col %in% columns) {
    cli::cli_abort(
      "{.arg data} must have a geometry column of type {.cls sfc}.",
      call = call
    )
  }
  if (identical(id, geom_col)) {
    cli::cli_abort(
      "{.arg id} cannot be the geometry column in {.arg data}.",
      call = call
    )
  }
  function(x) {
    if (inherits(x, "sfc")) {
      return(x)
    }
    matches <- vec_locate_matches(x, data[[id]])
    if (nrow(matches) < 1 || all(is.na(matches$haystack))) {
      cli::cli_abort(
        "No values match the {.var {id}} column in the spatial data.",
        call = call
      )
    }
    if (vec_duplicate_any(matches$needles)) {
      groups <- vec_split(data[[geom_col]][matches$haystack], matches$needles)
      vec_c(!!!lapply(groups$val, sf::st_combine))
    } else {
      data[[geom_col]][matches$haystack]
    }
  }
}
