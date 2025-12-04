property_boolean <- function(allow_null = FALSE, default = TRUE) {
  class <- S7::class_logical
  class <- if (allow_null) S7::new_union(class, NULL) else class
  validator <- function(value) {
    if ((allow_null && is.null(value)) || is_bool(value)) {
      return(character())
    }
    "must be a boolean"
  }
  S7::new_property(
    class = class,
    validator = validator,
    default = default
  )
}

property_choice <- function(options, allow_null = FALSE, default = NULL) {
  force(options)
  class <- S7::class_character
  class <- if (allow_null) S7::new_union(class, NULL) else class
  validator <- function(value) {
    if (allow_null && is.null(value)) {
      return(character())
    }
    if (!is_character(value)) {
      return(as_cli("must be a string, not {.obj_type_friendly {value}}"))
    }
    if (all(value %in% options)) {
      return(character())
    }
    as_cli("must be one of {.or {.val {options}}}")
  }
  S7::new_property(
    class = class,
    validator = validator,
    default = default
  )
}

# This is like `property_choice`, but allows for integers that mean `1 = plain`,
# `2 = bold`, `3 = italic`,  `4 = bold italic`.
property_fontface <- function(allow_null = TRUE, default = NULL) {
  options <- c("plain", "bold", "italic", "oblique", "bold.italic")
  class <- S7::new_union(S7::class_character, S7::class_numeric)
  class <- if (allow_null) S7::new_union(class, NULL) else class
  validator <- function(value) {
    if (allow_null && is.null(value)) {
      return(character())
    }
    if (is_integerish(value) && all(value %in% 1:4)) {
      return(character())
    }
    if (all(value %in% options)) {
      return(character())
    }
    as_cli("must be one of {.or {.val {options}}}.")
  }
  S7::new_property(
    class = class,
    validator = validator,
    default = default
  )
}

property_nullable <- function(class = S7::class_any, ...) {
  S7::new_property(
    class = S7::new_union(NULL, class),
    ...
  )
}

property_colour <- function(
  allow_null = TRUE,
  pattern = FALSE,
  default = NULL
) {
  # TODO: remove numeric option for editioning
  class <- S7::new_union(
    S7::class_character, # Hex codes and colour names, e.g. #FF000 or "red"
    S7::class_logical,   # For allowing NA, which means 'transparent'
    S7::class_numeric    # For `grDevices::palette()` indexing
  )
  if (isTRUE(pattern)) {
    class <- S7::new_union(class, S7::new_S3_class("GridPattern"))
  }
  if (isTRUE(allow_null)) {
    class <- S7::new_union(class, NULL)
  }
  validator <- function(value) {
    if (is.numeric(value) && !is_integerish(value)) {
      return("cannot be a decimal number, but could be an integer.")
    }
    character()
  }
  S7::new_property(
    class = class,
    validator = validator,
    default = default
  )
}

