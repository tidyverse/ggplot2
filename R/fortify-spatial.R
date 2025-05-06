#' Fortify method for classes from the sp package.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' To figure out the correct variable name for region, inspect
#' `as.data.frame(model)`.
#'
#' @param model `SpatialPolygonsDataFrame` to convert into a dataframe.
#' @param data not used by this method
#' @param region name of variable used to split up regions
#' @param ... not used by this method
#' @keywords internal
#' @name fortify.sp
NULL

#' @rdname fortify.sp
#' @export
#' @method fortify SpatialPolygonsDataFrame
fortify.SpatialPolygonsDataFrame <- function(model, data, region = NULL, ...) {
  deprecate_warn0("3.4.4",
    I("`fortify(<SpatialPolygonsDataFrame>)`"),
    details = "Please migrate to sf."
  )

  attr <- as.data.frame(model)
  # If not specified, split into regions based on polygons
  if (is.null(region)) {
    # Suppress duplicated warnings
    withr::with_options(list(lifecycle_verbosity = "quiet"), {
      coords <- lapply(model@polygons,fortify)
    })
    coords <- vec_rbind0(!!!coords)
    cli::cli_inform("Regions defined for each Polygons")
  } else {
    lifecycle::deprecate_stop("3.4.4",
      I("`fortify(<SpatialPolygonsDataFrame>, region = ...)` is defunct'"),
      details = "Please migrate to sf."
    )
  }
  coords
}

#' @rdname fortify.sp
#' @export
#' @method fortify SpatialPolygons
fortify.SpatialPolygons <- function(model, data, ...) {
  deprecate_warn0("3.4.4",
    I("`fortify(<SpatialPolygons>)`"),
    details = "Please migrate to sf."
  )

  # Suppress duplicated warnings
  withr::with_options(list(lifecycle_verbosity = "quiet"), {
    polys <- lapply(model@polygons, fortify)
  })
  vec_rbind0(!!!polys)
}

#' @rdname fortify.sp
#' @export
#' @method fortify Polygons
fortify.Polygons <- function(model, data, ...) {
  deprecate_warn0("3.4.4",
    I("`fortify(<Polygons>)`"),
    details = "Please migrate to sf."
  )

  subpolys <- model@Polygons
  pieces <- lapply(seq_along(subpolys), function(i) {
    df <- fortify(subpolys[[model@plotOrder[i]]])
    df$piece <- i
    df
  })
  pieces <- vec_rbind0(!!!pieces)

  pieces$order <- seq_len(nrow(pieces))
  pieces$id <- model@ID
  pieces$piece <- factor(pieces$piece)
  pieces$group <- interaction(pieces$id, pieces$piece)
  pieces
}

#' @rdname fortify.sp
#' @export
#' @method fortify Polygon
fortify.Polygon <- function(model, data, ...) {
  deprecate_warn0("3.4.4",
    I("`fortify(<Polygon>)`"),
    details = "Please migrate to sf."
  )

  df <- as.data.frame(model@coords)
  names(df) <- c("long", "lat")
  df$order <- seq_len(nrow(df))
  df$hole <- model@hole
  df
}

#' @rdname fortify.sp
#' @export
#' @method fortify SpatialLinesDataFrame
fortify.SpatialLinesDataFrame <- function(model, data, ...) {
  deprecate_warn0("3.4.4",
    I("`fortify(<SpatialLinesDataFrame>)`"),
    details = "Please migrate to sf."
  )

  lines <- lapply(model@lines, fortify)
  vec_rbind0(!!!lines)
}

#' @rdname fortify.sp
#' @export
#' @method fortify Lines
fortify.Lines <- function(model, data, ...) {
  deprecate_warn0("3.4.4",
    I("`fortify(<Lines>)`"),
    details = "Please migrate to sf."
  )

  lines <- model@Lines
  pieces <- lapply(seq_along(lines), function(i) {
    df <- fortify(lines[[i]])
    df$piece <- i
    df
  })
  pieces <- vec_rbind0(!!!pieces)

  pieces$order <- seq_len(nrow(pieces))
  pieces$id <- model@ID
  pieces$piece <- factor(pieces$piece)
  pieces$group <- interaction(pieces$id, pieces$piece)
  pieces
}

#' @rdname fortify.sp
#' @export
#' @method fortify Line
fortify.Line <- function(model, data, ...) {
  deprecate_warn0("3.4.4",
    I("`fortify(<Line>)`"),
    details = "Please migrate to sf."
  )

  df <- as.data.frame(model@coords)
  names(df) <- c("long", "lat")
  df$order <- seq_len(nrow(df))
  df
}


#' @export
#' @method fortify sfc
fortify.sfc <- function(model, data, ...) {
  sf::st_sf(geometry = model)
}

#' @export
#' @method fortify sfg
fortify.sfg <- function(model, data, ...) {
  sf::st_sf(geometry = sf::st_sfc(model))
}
