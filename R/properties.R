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

property_nullable <- function(class = S7::class_any, ...) {
  S7::new_property(
    class = S7::new_union(NULL, class),
    ...
  )
}

property_index <- function(i) {
  force(i)
  S7::new_property(
    getter = function(self) {
      self[i]
    },
    setter = function(self, value) {
      self[i] <- value
      self
    }
  )
}
