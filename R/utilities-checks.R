
# Extra checks in addition to the ones in import-standalone-types-check.R

# Usage:
# check_object(x, is.data.frame, "a data.frame)
check_object <- function(x,
                         check_fun,
                         what,
                         ...,
                         allow_null = FALSE,
                         arg = caller_arg(x),
                         call = caller_env()) {

  if (!missing(x)) {
    if (check_fun(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    as_cli(what),
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_numeric <- function(x,
                          what = "a {.cls numeric} vector",
                          ...,
                          arg = caller_arg(x),
                          call = caller_env()) {
  check_object(x, is.numeric, what, ..., arg = arg, call = call)
}

check_inherits <- function(x,
                           class,
                           what = NULL,
                           ...,
                           allow_null = FALSE,
                           arg = caller_arg(x),
                           call = caller_env()) {

  if (!missing(x)) {
    if (inherits(x, class)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  what <- what %||% paste(
    "a", oxford_comma(paste0("{.cls ", class, "}")), "object"
  )

  stop_input_type(
    x,
    as_cli(what),
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

#' Check graphics device capabilities
#'
#' This function makes an attempt to estimate whether the graphics device is
#' able to render newer graphics features.
#'
#' @param feature A string naming a graphics device feature. One of:
#'   `"clippingPaths"`, `"alpha_masks"`, `"lumi_masks"`, `"compositing"`,
#'   `"blending"`, `"transformations"`, `"gradients"`, `"patterns"`, `"paths"`
#'   or `"glyphs"`. See the 'Features' section below for an explanation
#'   of these terms.
#' @param action A string for what action to take. One of:
#'   * `"test"` returns `TRUE` or `FALSE` indicating support of the feature.
#'   * `"warn"` also returns a logical, but throws an informative warning when
#'   `FALSE`.
#'   * `"abort"` throws an error when the device is estimated to not support
#'   the feature.
#' @param op A string for a specific operation to test for when `feature` is
#'   either `"blending"` or `"compositing"`. If `NULL` (default), support for
#'   all known blending or compositing operations is queried.
#' @param maybe A logical of length 1 determining what the return value should
#'   be in case the device capabilities cannot be assessed.
#' @param call The execution environment of a currently running function, e.g.
#'   [`caller_env()`][rlang::caller_env()]. The function will be mentioned in
#'   warnings and error messages as the source of the warning or error. See
#'   the `call` argument of [`abort()`][rlang::abort()] for more information.
#'
#' @details
#' The procedure for testing is as follows:
#'
#' * First, the \R version is checked against the version wherein a feature was
#'   introduced.
#' * Next, the [dev.capabilities()][grDevices::dev.capabilities()] function is
#'   queried for support of the feature.
#' * If that check is ambiguous, the \pkg{svglite} and \pkg{ragg} devices are
#'   checked for known support.
#' * Lastly, if there is no answer yet, it is checked whether the device is one
#'   of the 'known' devices that supports a feature.
#'
#' @section Features:
#' \describe{
#'   \item{`"clippingPaths"`}{While most devices support rectangular clipping
#'   regions, this feature is about the support for clipping to arbitrary paths.
#'   It can be used to only display a part of a drawing.}
#'   \item{`"alpha_masks"`}{Like clipping regions and paths, alpha masks can also
#'   be used to only display a part of a drawing. In particular a
#'   semi-transparent mask can be used to display a drawing in the opaque parts
#'   of the mask and hide a drawing in transparent part of a mask.}
#'   \item{`"lumi_masks`}{Similar to alpha masks, but using the mask's luminance
#'   (greyscale value) to determine what is drawn. Light values are opaque and
#'   dark values are transparent.}
#'   \item{`"compositing"`}{Compositing allows one to control how to drawings
#'   are drawn in relation to one another. By default, one drawing is drawn
#'   'over' the previous one, but other operators are possible, like 'clear',
#'   'in' and 'out'.}
#'   \item{`"blending"`}{When placing one drawing atop of another, the blend
#'   mode determines how the colours of the drawings relate to one another.}
#'   \item{`"transformations"`}{Performing an affine transformation on a group
#'   can be used to translate, rotate, scale, shear and flip the drawing.}
#'   \item{`"gradients"`}{Gradients can be used to show a transition between
#'   two or more colours as a fill in a drawing. The checks expects both linear
#'   and radial gradients to be supported.}
#'   \item{`"patterns"`}{Patterns can be used to display a repeated, tiled
#'   drawing as a fill in another drawing.}
#'   \item{`"paths"`}{Contrary to 'paths' as polyline or polygon drawings,
#'   `"paths"` refers to the ability to fill and stroke collections of
#'   drawings.}
#'   \item{`"glyphs"`}{Refers to the advanced typesetting feature for
#'   controlling the appearance of individual glyphs.}
#' }
#'
#' @section Limitations:
#'
#' * On Windows machines, bitmap devices such as `png()` or `jpeg()` default
#'   to `type = "windows"`. At the time of writing, these don't support any
#'   new features, in contrast to `type = "cairo"`, which does. Prior to \R
#'   version 4.2.0, the capabilities cannot be resolved and the value of the
#'   `maybe` argument is returned.
#' * With the exception of the \pkg{ragg} and \pkg{svglite} devices, if the
#'   device doesn't report their capabilities via
#'   [dev.capabilities()][grDevices::dev.capabilities()], or the \R version is
#'   below 4.2.0, the `maybe` value is returned.
#' * Even though patterns and gradients where introduced in \R 4.1.0, they
#'   are considered unsupported because providing vectorised patterns and
#'   gradients was only introduced later in \R 4.2.0.
#' * When using the RStudio graphics device, the back end is assumed to be the
#'   next device on the list. This assumption is typically met by default,
#'   unless the device list is purposefully rearranged.
#'
#' @return `TRUE` when the feature is thought to be supported and `FALSE`
#'   otherwise.
#' @export
#' @keywords internal
#'
#' @examples
#' # Typically you'd run `check_device()` inside a function that might produce
#' # advanced graphics.
#' # The check is designed for use in control flow statements in the test mode
#' if (check_device("patterns", action = "test")) {
#'   print("Yay")
#' } else {
#'   print("Nay")
#' }
#'
#' # Automatically throw a warning when unavailable
#' if (check_device("compositing", action = "warn")) {
#'   print("Yay")
#' } else {
#'   print("Nay")
#' }
#'
#' # Possibly throw an error
#' try(check_device("glyphs", action = "abort"))
check_device = function(feature, action = "warn", op = NULL, maybe = FALSE,
                        call = caller_env()) {

  check_bool(maybe, allow_na = TRUE)

  action <- arg_match0(action, c("test", "warn", "abort"))
  action_fun <- switch(
    action,
    warn  = cli::cli_warn,
    abort = cli::cli_abort,
    function(...) invisible()
  )

  feature <- arg_match0(
    feature,
    c("clippingPaths", "alpha_masks", "lumi_masks", "compositing", "blending",
      "transformations", "glyphs", "patterns", "gradients", "paths",
      ".test_feature")
  )
  # Formatting prettier feature names
  feat_name <- switch(
    feature,
    clippingPaths   = "clipping paths",
    patterns        = "tiled patterns",
    blending        = "blend modes",
    gradients       = "colour gradients",
    glyphs          = "typeset glyphs",
    paths           = "stroking and filling paths",
    transformations = "affine transformations",
    alpha_masks     = "alpha masks",
    lumi_masks      = "luminance masks",
    feature
  )

  # Perform version check
  version <- getRversion()
  capable <- switch(
    feature,
    glyphs = version >= "4.3.0",
    paths =, transformations =, compositing =,
    patterns =, lumi_masks =, blending =,
    gradients = version >= "4.2.0",
    alpha_masks =,
    clippingPaths = version >= "4.1.0",
    TRUE
  )
  if (isFALSE(capable)) {
    action_fun("R {version} does not support {.emph {feature}}.",
               call = call)
    return(FALSE)
  }

  # Grab device for checking
  dev_cur  <- grDevices::dev.cur()
  dev_name <- names(dev_cur)

  if (dev_name == "RStudioGD") {
    # RStudio opens RStudioGD as the active graphics device, but the back-end
    # appears to be the *next* device. Temporarily set the next device as the
    # device to check capabilities.
    dev_old <- dev_cur
    on.exit(grDevices::dev.set(dev_old), add = TRUE)
    dev_cur  <- grDevices::dev.set(grDevices::dev.next())
    dev_name <- names(dev_cur)
  }

  # For blending/compositing, maybe test a specific operation
  if (!is.null(op) && feature %in% c("blending", "compositing")) {
    op <- arg_match0(op, c(.blend_ops, .compo_ops))
    .blend_ops <- .compo_ops <- op
    feat_name <- paste0("'", gsub("\\.", " ", op), "' ", feat_name)
  }

  # The dev.capabilities() approach may work from R 4.2.0 onwards
  if (version >= "4.2.0") {
    capa <- grDevices::dev.capabilities()

    # Test if device explicitly states that it is capable of this feature
    capable <- switch(
      feature,
      clippingPaths = isTRUE(capa$clippingPaths),
      gradients = all(c("LinearGradient", "RadialGradient") %in% capa$patterns),
      alpha_masks = "alpha" %in% capa$masks,
      lumi_masks = "luminance" %in% capa$masks,
      patterns = "TilingPattern" %in% capa$patterns,
      compositing = all(.compo_ops %in% capa$compositing),
      blending = all(.blend_ops %in% capa$compositing),
      transformations = isTRUE(capa$transformations),
      paths = isTRUE(capa$paths),
      glyphs = isTRUE(capa$glyphs),
      NA
    )
    if (isTRUE(capable)) {
      return(TRUE)
    }

    # Test if device explicitly denies that it is capable of this feature
    incapable <- switch(
      feature,
      clippingPaths = isFALSE(capa$clippingPaths),
      gradients = !all(is.na(capa$patterns)) &&
        !all(c("LinearGradient", "RadialGradient") %in% capa$patterns),
      alpha_masks = !is.na(capa$masks) && !("alpha" %in% capa$masks),
      lumi_masks  = !is.na(capa$masks) && !("luminance" %in% capa$masks),
      patterns = !is.na(capa$patterns) && !("TilingPattern" %in% capa$patterns),
      compositing = !all(is.na(capa$compositing)) &&
        !all(.compo_ops %in% capa$compositing),
      blending = !all(is.na(capa$compositing)) &&
        !all(.blend_ops %in% capa$compositing),
      transformations = isFALSE(capa$transformations),
      paths = isFALSE(capa$paths),
      glyphs = isFALSE(capa$glyphs),
      NA
    )

    if (isTRUE(incapable)) {
      action_fun(
        "The {.field {dev_name}} device does not support {.emph {feat_name}}.",
        call = call
      )
      return(FALSE)
    }
  }

  # Test {ragg}'s capabilities
  if (dev_name %in% c("agg_jpeg", "agg_ppm", "agg_png", "agg_tiff")) {
    # We return ragg's version number if not installed, so we can suggest to
    # install it.
    capable <- switch(
      feature,
      clippingPaths =, alpha_masks =, gradients =,
      patterns = if (is_installed("ragg", version = "1.2.0")) TRUE else "1.2.0",
      FALSE
    )
    if (isTRUE(capable)) {
      return(TRUE)
    }
    if (is.character(capable) && action != "test") {
      check_installed(
        "ragg", version = capable,
        reason = paste0("for graphics support of ", feat_name, ".")
      )
    }
    action_fun(paste0(
      "The {.pkg ragg} package's {.field {dev_name}} device does not support ",
      "{.emph {feat_name}}."
    ), call = call)
    return(FALSE)
  }

  # The vdiffr version of the SVG device is known to not support any newer
  # features
  if (dev_name == "devSVG_vdiffr") {
    action_fun(
      "The {.pkg vdiffr} package's device does not support {.emph {feat_name}}.",
      call = call
    )
    return(FALSE)
  }

  # The same logic applies to {svglite} but is tested separately in case
  # {ragg} and {svglite} diverge at some point.
  if (dev_name == "devSVG") {
    # We'll return a version number if not installed so we can suggest it
    capable <- switch(
      feature,
      clippingPaths =, gradients =, alpha_masks =,
      patterns = if (is_installed("svglite", version = "2.1.0")) TRUE else "2.1.0",
      FALSE
    )

    if (isTRUE(capable)) {
      return(TRUE)
    }
    if (is.character(capable) && action != "test") {
      check_installed(
        "svglite", version = capable,
        reason = paste0("for graphics support of ", feat_name, ".")
      )
    }
    action_fun(paste0(
      "The {.pkg {pkg}} package's {.field {dev_name}} device does not ",
      "support {.emph {feat_name}}."), call = call
    )
    return(FALSE)
  }

  # Last resort: list of known support prior to R 4.2.0
  supported <- c("pdf", "cairo_pdf", "cairo_ps", "svg")
  if (feature == "compositing") {
    supported <- setdiff(supported, "pdf")
  }
  if (.Platform$OS.type == "unix") {
    # These devices *can* be supported on Windows, but would have to have
    # type = "cairo", which we can't check.
    supported <- c(supported, "bmp", "jpeg", "png", "tiff")
  }
  if (isTRUE(dev_name %in% supported)) {
    return(TRUE)
  }
  action_fun(
    "Unable to check the capabilities of the {.field {dev_name}} device.",
    call = call
  )
  return(maybe)
}

.compo_ops <- c("clear", "source", "over", "in", "out", "atop", "dest",
                "dest.over", "dest.in", "dest.out", "dest.atop", "xor", "add",
                "saturate")

.blend_ops <- c("multiply", "screen", "overlay", "darken", "lighten",
                "color.dodge", "color.burn", "hard.light", "soft.light",
                "difference", "exclusion")
