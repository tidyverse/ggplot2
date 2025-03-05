
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
    if (!is_string(value)) {
      return(as_cli("must be a string, not {.obj_type_friendly {value}}"))
    }
    if (value %in% options) {
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

element_props <- list(
  fill = S7::new_property(
    S7::new_union(S7::class_character, S7::new_S3_class("GridPattern"), S7::class_logical, NULL),
    default = NULL
  ),
  colour = S7::new_property(
    S7::new_union(S7::class_character, S7::class_logical, NULL),
    default = NULL
  ),
  family = S7::new_property(
    S7::new_union(S7::class_character, NULL),
    default = NULL
  ),
  hjust = S7::new_property(
    S7::new_union(S7::class_numeric, NULL),
    default = NULL
  ),
  vjust = S7::new_property(
    S7::new_union(S7::class_numeric, NULL),
    default = NULL
  ),
  angle = S7::new_property(
    S7::new_union(S7::class_numeric, NULL),
    default = NULL
  ),
  size = S7::new_property(
    S7::new_union(S7::class_numeric, NULL),
    default = NULL
  ),
  lineheight = S7::new_property(
    S7::new_union(S7::class_numeric, NULL),
    default = NULL
  ),
  margin = S7::new_property(
    S7::new_union(S7::new_S3_class("margin"), NULL),
    default = NULL
  ),
  face = property_choice(c("plain", "bold", "italic", "oblique", "bold.italic"), allow_null = TRUE),
  linewidth = S7::new_property(
    S7::new_union(S7::class_numeric, NULL),
    default = NULL
  ),
  linetype = S7::new_property(
    S7::new_union(S7::class_numeric, S7::class_character, NULL),
    default = NULL
  ),
  lineend = property_choice(c("round", "butt", "square"), allow_null = TRUE),
  shape = S7::new_property(
    S7::new_union(S7::class_numeric, S7::class_character, NULL),
    default = NULL
  ),
  arrow = S7::new_property(
    S7::new_union(S7::new_S3_class("arrow"), S7::class_logical, NULL),
    default = NULL
  ),
  arrow.fill = S7::new_property(
    S7::new_union(S7::class_character, S7::class_logical, NULL),
    default = NULL
  ),
  debug = property_boolean(allow_null = TRUE, default = NULL),
  inherit.blank = property_boolean(default = FALSE)
)
