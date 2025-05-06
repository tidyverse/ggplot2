#' Update axis/legend labels
#'
#' @param p plot to modify
#' @param labels named list of new labels
#' @keywords internal
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' update_labels(p, list(x = "New x"))
#' update_labels(p, list(x = expression(x / y ^ 2)))
#' update_labels(p, list(x = "New x", y = "New Y"))
#' update_labels(p, list(colour = "Fail silently"))
update_labels <- function(p, labels) {
  p <- plot_clone(p)
  p$labels <- defaults(labels, p$labels)
  p
}

# Called in `ggplot_build()` to set default labels not specified by user.
setup_plot_labels <- function(plot, layers, data) {
  # Initiate empty labels
  labels <- list()

  # Find labels from every layer
  for (i in seq_along(layers)) {
    layer <- layers[[i]]

    mapping <- layer$computed_mapping
    if (inherits(mapping, "unlabelled_uneval")) {
      next
    }

    mapping <- strip_stage(mapping)
    mapping <- strip_dots(mapping, strip_pronoun = TRUE)

    exclude <- names(layer$aes_params)
    mapping <- mapping[setdiff(names(mapping), exclude)]

    # Acquire default labels
    mapping_default <- make_labels(mapping)
    stat_default <- lapply(
      make_labels(layer$stat$default_aes),
      function(l) {
        attr(l, "fallback") <- TRUE
        l
      }
    )
    default <- defaults(mapping_default, stat_default)

    # Search for label attribute in symbolic mappings
    symbolic <- vapply(
      mapping, FUN.VALUE = logical(1),
      function(x) is_quosure(x) && quo_is_symbol(x)
    )
    symbols <- intersect(names(mapping)[symbolic], names(data[[i]]))
    attribs <- lapply(setNames(nm = symbols), function(x) {
      attr(data[[i]][[x]], "label", exact = TRUE)
    })
    attribs <- attribs[lengths(attribs) > 0]
    layer_labels <- defaults(attribs, default)

    # Set label priority:
    # 1. Existing labels that aren't fallback labels
    # 2. The labels of this layer, including fallback labels
    # 3. Existing fallback labels
    current <- labels
    fallbacks <- vapply(current, function(l) isTRUE(attr(l, "fallback")), logical(1))

    labels <- defaults(current[!fallbacks], layer_labels)
    if (any(fallbacks)) {
      labels <- defaults(labels, current)
    }
  }

  # Warn for spurious labels that don't have a mapping.
  # Note: sometimes, 'x' and 'y' might not have a mapping, like in
  # `geom_function()`. We can display these labels anyway, so we include them.
  plot_labels  <- plot$labels
  known_labels <- c(names(labels), fn_fmls_names(labs), "x", "y")
  extra_labels <- setdiff(names(plot_labels), known_labels)

  if (length(extra_labels) > 0) {
    extra_labels <- paste0(
      "{.code ", extra_labels, " = \"", plot_labels[extra_labels], "\"}"
    )
    names(extra_labels) <- rep("*", length(extra_labels))
    cli::cli_warn(c(
      "Ignoring unknown labels:",
      extra_labels
    ))
  }

  # User labels can be functions, so apply these to the default labels
  plot_labels <- lapply(setNames(nm = names(plot_labels)), function(nm) {
    label <- plot_labels[[nm]]
    if (!is.function(label)) {
      return(label)
    }
    label(labels[[nm]] %||% "")
  })

  dict <- plot_labels$dictionary
  if (length(dict) > 0) {
    labels <- lapply(labels, function(x) {
      dict <- dict[names(dict) %in% x]
      x[match(names(dict), x)] <- dict
      x
    })
  }

  defaults(plot_labels, labels)
}

#' Modify axis, legend, and plot labels
#'
#' Good labels are critical for making your plots accessible to a wider
#' audience. Always ensure the axis and legend labels display the full
#' variable name. Use the plot `title` and `subtitle` to explain the
#' main findings. It's common to use the `caption` to provide information
#' about the data source. `tag` can be used for adding identification tags
#' to differentiate between multiple plots.
#'
#' You can also set axis and legend labels in the individual scales (using
#' the first argument, the `name`). If you're changing other scale options, this
#' is recommended.
#'
#' If a plot already has a title, subtitle, caption, etc., and you want to
#' remove it, you can do so by setting the respective argument to `NULL`. For
#' example, if plot `p` has a subtitle, then `p + labs(subtitle = NULL)` will
#' remove the subtitle from the plot.
#'
#' @param label The title of the respective axis (for `xlab()` or `ylab()`) or
#'        of the plot (for `ggtitle()`).
#' @param title The text for the title.
#' @param subtitle The text for the subtitle for the plot which will be
#'        displayed below the title.
#' @param caption The text for the caption which will be displayed in the
#'        bottom-right of the plot by default.
#' @param tag The text for the tag label which will be displayed at the
#'        top-left of the plot by default.
#' @param dictionary A named character vector to serve as dictionary.
#'        Automatically derived labels, such as those based on variables will
#'        be matched with `names(dictionary)` and replaced by the matching
#'        entry in `dictionary`.
#' @param alt,alt_insight Text used for the generation of alt-text for the plot.
#'        See [get_alt_text] for examples. `alt` can also be a function that
#'        takes the plot as input and returns text as output. `alt` also accepts
#'        rlang [lambda][rlang::as_function()] function notation.
#' @param ... A list of new name-value pairs. The name should be an aesthetic.
#' @export
#'
#' @seealso
#' The `r link_book("plot and axis titles section", "annotations#sec-titles")`
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()
#' p + labs(colour = "Cylinders")
#' p + labs(x = "New x label")
#'
#' # Set labels by variable name instead of aesthetic
#' p + labs(dict = c(
#'   disp = "Displacment", # Not in use
#'   cyl  = "Number of cylinders",
#'   mpg  = "Miles per gallon",
#'   wt   = "Weight (1000 lbs)"
#' ))
#'
#' # The plot title appears at the top-left, with the subtitle
#' # display in smaller text underneath it
#' p + labs(title = "New plot title")
#' p + labs(title = "New plot title", subtitle = "A subtitle")
#'
#' # The caption appears in the bottom-right, and is often used for
#' # sources, notes or copyright
#' p + labs(caption = "(based on data from ...)")
#'
#' # The plot tag appears at the top-left, and is typically used
#' # for labelling a subplot with a letter.
#' p + labs(title = "title", tag = "A")
#'
#' # If you want to remove a label, set it to NULL.
#' p +
#'  labs(title = "title") +
#'  labs(title = NULL)
labs <- function(..., title = waiver(), subtitle = waiver(), caption = waiver(),
                 tag = waiver(), dictionary = waiver(), alt = waiver(),
                 alt_insight = waiver()) {
  # .ignore_empty = "all" is needed to allow trailing commas, which is NOT a trailing comma for dots_list() as it's in ...
  args <- dots_list(..., title = title, subtitle = subtitle, caption = caption,
    tag = tag, alt = allow_lambda(alt), alt_insight = alt_insight,
    dictionary = dictionary, .ignore_empty = "all")

  is_waive <- vapply(args, is.waiver, logical(1))
  args <- args[!is_waive]
  # remove duplicated arguments
  args <- args[!duplicated(names(args))]
  args <- rename_aes(args)

  structure(args, class = c("labels", "gg"))
}

#' @rdname labs
#' @export
xlab <- function(label) {
  labs(x = label)
}

#' @rdname labs
#' @export
ylab <- function(label) {
  labs(y = label)
}

#' @rdname labs
#' @export
ggtitle <- function(label, subtitle = waiver()) {
  labs(title = label, subtitle = subtitle)
}

#' @rdname labs
#' @export
#' @param plot A ggplot object
#' @description
#' `get_labs()` retrieves completed labels from a plot.
get_labs <- function(plot = get_last_plot()) {
  plot <- ggplot_build(plot)

  labs <- plot$plot$labels

  xy_labs <- rename(
    c(x = plot$layout$resolve_label(plot$layout$panel_scales_x[[1]], labs),
      y = plot$layout$resolve_label(plot$layout$panel_scales_y[[1]], labs)),
    c(x.primary = "x", x.secondary = "x.sec",
      y.primary = "y", y.secondary = "y.sec")
  )

  labs <- defaults(xy_labs, labs)

  guides <- plot$plot$guides
  if (length(guides$aesthetics) == 0) {
    return(labs)
  }

  for (aes in guides$aesthetics) {
    param <- guides$get_params(aes)
    aes   <- param$aesthetic # Can have length > 1 when guide was merged
    title <- vec_set_names(rep(list(param$title), length(aes)), aes)
    labs  <- defaults(title, labs)
  }
  labs
}

#' Extract alt text from a plot
#'
#' This function returns a text that can be used as alt-text in webpages etc.
#' Currently it will use the `alt` label, added with `+ labs(alt = <...>)`, or
#' a return an empty string, but in the future it might try to generate an alt
#' text from the information stored in the plot.
#'
#' @param p a ggplot object
#' @inheritParams rlang::args_dots_used
#'
#' @return A text string
#'
#' @export
#' @aliases alt_text
#'
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#'
#' # Returns an empty string
#' get_alt_text(p)
#'
#' # A user provided alt text
#' p <- p + labs(
#'   alt = paste("A scatterplot showing the negative correlation between engine",
#'               "displacement as a function of highway miles per gallon")
#' )
#'
#' get_alt_text(p)
#'
get_alt_text <- function(p, ...) {
  warn_dots_used()
  UseMethod("get_alt_text")
}
#' @export
get_alt_text.ggplot <- function(p, ...) {
  alt <- p$labels[["alt"]] %||% ""
  if (!is.function(alt)) {
    return(alt)
  }
  p$labels[["alt"]] <- NULL
  build <- ggplot_build(p)
  build$plot$labels[["alt"]] <- alt
  get_alt_text(build)
}
#' @export
get_alt_text.ggplot_built <- function(p, ...) {
  alt <- p$plot$labels[["alt"]] %||% ""
  p$plot$labels[["alt"]] <- NULL
  if (is.function(alt)) alt(p$plot) else alt
}
#' @export
get_alt_text.gtable <- function(p, ...) {
  attr(p, "alt-label") %||% ""
}

#' Generate an alt text from a plot
#'
#' This function returns a text that can be used as alt-text in webpages etc.
#' It will synthesize one from the information in the plot itself, but you can
#' add a conclusion to the synthesized text using `+ labs(alt_insight = <...>)`.
#'
#' There is no way an automatically generated description can compete with one
#' written by a human with knowledge of what the plot shows and in which
#' context. We urge users to write their own alt text if at all possible.
#' Guidance to how an effective alt-text is written can be found in
#' [Writing Alt Text for Data Visualization](https://medium.com/nightingale/writing-alt-text-for-data-visualization-2a218ef43f81)
#' and [Effective Practices for Description of Science Content within Digital Talking Books](https://www.wgbh.org/foundation/ncam/guidelines/effective-practices-for-description-of-science-content-within-digital-talking-books)
#'
#' @param p a ggplot object
#'
#' @return A text string
#'
#' @noRd
#'
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#'
#' get_alt_text(p)
#'
#' p <- p + ggtitle("The relationship between displacement and yield in cars")
#' get_alt_text(p)
#'
#' # It will use scale information if available
#' p <- p + scale_x_continuous("highway miles per gallon")
#' get_alt_text(p)
#'
#' # Add a short description of the main conclusion of the plot
#' p <- p + labs(alt_insight = "The higher the yield, the lower the displacement")
#' get_alt_text(p)
#'
#' # A user provided alt text takes precedence
#' p <- p + labs(
#'   alt = paste("A scatterplot showing the negative correlation between engine",
#'               "displacement as a function of highway miles per gallon")
#' )
#'
#' get_alt_text(p)
#'
generate_alt_text <- function(p) {
  # Combine titles
  if (!is.null(p$label$title %||% p$labels$subtitle)) {
    title <- sub("\\.?$", "", c(p$labels$title, p$labels$subtitle))
    if (length(title) == 2) {
      title <- paste0(title[1], ": ", title[2])
    }
    title <- paste0(title, ". ")
    title <- safe_string(title)
  } else {
    title <- ""
  }


  # Get axes descriptions
  axes <- paste0(" showing ", scale_description(p, "x"), " and ", scale_description(p, "y"))
  axes <- safe_string(axes)

  # Get layer types
  layers <- vapply(p$layers, function(l) snake_class(l$geom), character(1))
  layers <- sub("_", " ", sub("^geom_", "", unique0(layers)))
  if (length(layers) == 1) {
    layers <- paste0(" using a ", layers, " layer")
  } else {
    layers <- paste0(" using ", oxford_comma(layers), " layers")
  }
  layers <- safe_string(layers)

  # Combine
  alt <- paste0(title, "A plot", axes, layers, ".")
  if (!is.null(p$labels$alt_insight)) {
    alt <- paste0(alt, " ", p$labels$alt_insight)
  }
  as.character(alt)
}
safe_string <- function(string) {
  if (length(string) == 0) "" else string
}
scale_description <- function(p, name) {
  scale <- p$scales$get_scales(name)
  if (is.null(scale)) {
    lab <- p$labels[[name]]
    type <- "the"
  } else {
    lab <- scale$make_title(scale$name %|W|% p$labels[[name]])
    type <- "a continuous"
    if (scale$is_discrete()) type <- "a discrete"
    if (inherits(scale, "ScaleBinned")) type <- "a binned"
  }
  if (is.null(lab)) {
    return(NULL)
  }
  paste0(lab, " on ", type, " ", name, "-axis")
}
