# Use extra_note arg to add some notes (e.g. the document is shared with multiple
# Geoms and there's some difference among their aesthetics).
rd_aesthetics <- function(type, name, extra_note = NULL) {
  obj <- switch(type,
    geom = check_subclass(name, "Geom", env = globalenv()),
    stat = check_subclass(name, "Stat", env = globalenv())
  )
  aes <- rd_aesthetics_item(obj)

  c(
    "@section Aesthetics:",
    paste0(
      "\\code{", type, "_", name, "()} ",
      "understands the following aesthetics. Required aesthetics are displayed in bold and optional aesthetics list their default values."
    ),
    "\\itemize{",
    paste0("  \\item ", aes),
    "}",
    if (!is.null(extra_note)) paste0(extra_note, "\n"),
    "Learn more about setting these aesthetics in \\code{vignette(\"ggplot2-specs\")}."
  )
}

# make the default value of an aesthetic displayable in the docs
rd_default_value_by_aesthetic <- function(aes, obj) {
  default_value <- obj$default_aes[aes]

  get_shape_name <- function(value) {
    # copied from geom-point.R
    pch_table <- c(
      "square open"           = 0,
      "circle open"           = 1,
      "triangle open"         = 2,
      "plus"                  = 3,
      "cross"                 = 4,
      "diamond open"          = 5,
      "triangle down open"    = 6,
      "square cross"          = 7,
      "asterisk"              = 8,
      "diamond plus"          = 9,
      "circle plus"           = 10,
      "star"                  = 11,
      "square plus"           = 12,
      "circle cross"          = 13,
      "square triangle"       = 14,
      "triangle square"       = 14,
      "square"                = 15,
      "circle small"          = 16,
      "triangle"              = 17,
      "diamond"               = 18,
      "circle"                = 19,
      "bullet"                = 20,
      "circle filled"         = 21,
      "square filled"         = 22,
      "diamond filled"        = 23,
      "triangle filled"       = 24,
      "triangle down filled"  = 25
    )
    name <- names(pch_table)[pch_table==value]
    glue('\\code{{{value}}} or \\code{{"{name}"}}')
  }

  get_linetype_name <- function(value) {
    linetype_table <- c(blank = 0, solid = 1, dashed = 2, dotted = 3, dotdash = 4, longdash = 5, twodash = 6)
    value = value[[1]]

    ifelse(is.numeric(value), glue('\\code{{{value}}} or \\code{{"{names(linetype_table)[linetype_table==value]}"}}'),
    ifelse(value %in% names(linetype_table), glue('\\code{{{linetype_table[value]}}} or \\code{{"{value}"}}'),
      glue('\\code{{"{value}"}}')))
  }

  get_styled_color = function(value) {
    if(is.na(value) || value == "transparent" || substring(as.character(value), 1, 1) == "~")
      return(glue('\\code{{{value}}}'))

    hex_string <- alpha(value)
    color <- col2rgb(value)
    text_color_string <- ifelse(sum(color * c(0.299, 0.587, 0.114)) < 128, "#FFFFFF", "#000000")

    # do not use \\code{} because it will override the text color settings
    glue('"<span style="color:{text_color_string}; background:{hex_string}; font-family:\'Lucida Console\', monospace !important;">{value}</span>"')
  }

  ifelse(aes == "shape", get_shape_name(default_value),
  ifelse(aes == "linetype", get_linetype_name(default_value),
  ifelse(aes %in% c("colour", "fill", "border_colour"),
         ifelse(is.na(default_value), '\\code{{NA}} (equivalent to \\code{"transparent"})', get_styled_color(default_value)),
  ifelse(aes == "alpha" & is.na(default_value), '\\code{NA} (equivalent to \\code{1})',
  ifelse(is.character(default_value[[1]]), glue('\\code{{"{default_value}"}}'),
    glue('\\code{{{as.character(default_value)}}}'))))))
}

rd_aesthetics_item <- function(x) {
  req <- x$required_aes
  req <- sub("|", "} \\emph{or} \\code{", req, fixed = TRUE)
  req_aes <- unlist(strsplit(x$required_aes, "|", fixed = TRUE))
  optional_aes <- setdiff(x$aesthetics(), req_aes)
  all <- union(req, sort(optional_aes))
  docs <- rd_match_docpage(all)

  default_values <- ifelse(all %in% names(x$default_aes),
    paste0(": ", sapply(all, rd_default_value_by_aesthetic, obj=x)),
    ""
  )

  item <- ifelse(all %in% req,
    paste0("\\strong{\\code{", docs, "}}"),
    paste0("\\code{", docs, "}", default_values)
  )
}

rd_match_docpage <- function(aes) {

  split <- strsplit(aes, "} \\emph{or} \\code{", fixed = TRUE)
  flat  <- unlist(split)

  index <- match(
    flat,
    c(
      "x", "y", "xmin", "xmax", "ymin", "ymax", "xend", 'yend',
      "colour", "fill", "alpha",
      "group",
      "linetype", "size", "shape", "linewidth"
    ), nomatch = 0L
  )
  docpage <- c(
    "", rep("aes_position", 8), rep("aes_colour_fill_alpha", 3),
    "aes_group_order", rep("aes_linetype_size_shape", 4)
  )[index + 1]
  no_match <- index == 0
  docpage[!no_match] <- paste0(
    "\\link[=", docpage[!no_match],
    "]{", flat[!no_match], "}"
  )
  docpage[no_match] <- flat[no_match]
  docpage <- split(docpage, rep(seq_along(split), lengths(split, FALSE)))
  vapply(docpage, paste, character(1), collapse = "} \\emph{or} \\code{")
}

rd_orientation <- function() {
  c(
    "@section Orientation: ",
    paste(
      'This geom treats each axis differently and, thus, can thus have two orientations.',
      'Often the orientation is easy to deduce from a combination of the given',
      'mappings and the types of positional scales in use. Thus, ggplot2 will by',
      'default try to guess which orientation the layer should have. Under rare',
      'circumstances, the orientation is ambiguous and guessing may fail. In that',
      'case the orientation can be specified directly using the \\code{orientation} parameter,',
      'which can be either \\code{"x"} or \\code{"y"}. The value gives the axis that the geom',
      'should run along, \\code{"x"} being the default orientation you would expect for the geom.'
    )
  )
}

#' Format 'Computed variables' section
#'
#' This is a helper function that helps format the 'Computed variables' section
#' of stat documentation pages. Briefly, it points to the delayed evaluation
#' documentation and it wraps the variable names in
#' 'after_stat()'.
#'
#' @param ... Named variable - description pairs. Variable names get wrapped in
#'   `after_stat()`. Variable names may be of the form "x|y" or "x,y" to format
#'   as "`after_stat(x)` or `after_stat(y)`" and
#'   "`after_stat(x)`, `after_stat(y)`" respectively.
#' @param .details Additional details to include in the preamble.
#' @param .skip_intro A logical, which if `TRUE` omits the link to the delayed
#'   evaluation docs. Can be useful when documenting two stat functions on the
#'   same page with different arguments, but no need to repeat the introduction
#'   in the second stat function (see e.g. `?stat_qq`).
#'
#' @return Formatted code that can be inserted into a .rd file.
#' @keywords internal
#' @noRd
#' @examples
#' rd_computed_vars(
#'   .details = "`stat_foobar()` computes the following variables:",
#'   "foo|bar" = "foobar",
#'   "bar,qux" = "quux",
#'   corge = "grault"
#' )
rd_computed_vars <- function(..., .details = "", .skip_intro = FALSE) {
  args  <- list(...)
  items <- names(args)
  descr <- unname(args)

  # Format preamble
  header <- "@section Computed variables: "
  intro  <- paste0(
    "These are calculated by the 'stat' part of layers and can be accessed ",
    "with [delayed evaluation][aes_eval]. "
  )
  if (.skip_intro) intro <- ""
  preamble <- c(header, paste0(intro, gsub("\n", "", .details)))

  # Format items
  fmt_items <- gsub(",", ")`, `after_stat(", items, fixed = TRUE)
  fmt_items <- gsub("|", ")` *or* `after_stat(",
                    fmt_items, fixed = TRUE)
  fmt_items <- paste0("*  `after_stat(", fmt_items, ")`")

  # Compose item-list
  fmt_descr <- gsub("\n", "", descr)
  fmt_list  <- paste(fmt_items, fmt_descr, sep = "\\cr ")

  c(preamble, fmt_list)
}
