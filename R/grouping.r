# This needs to be less than 1, to distinguish it from "regular" return values
# of plyr::id() used by add_group()
NO_GROUP <- -1L

# Ensure that the data frame contains a grouping variable.
#
# If the \code{group} variable is not present, then a new group
# variable is generated from the interaction of all discrete (factor or
# character) vectors, excluding \code{label}. The special value \code{NO_GROUP}
# is used for all observations if no discrete variables exist.
add_group <- function(data) {
  if (empty(data)) return(data)

  if (is.null(data$group)) {
    disc <- vapply(data, is.discrete, logical(1))
    disc[names(disc) %in% c("label", "PANEL")] <- FALSE

    if (any(disc)) {
      data$group <- plyr::id(data[disc], drop = TRUE)
    } else {
      data$group <- NO_GROUP
    }
  } else {
    data$group <- plyr::id(data["group"], drop = TRUE)
  }

  data
}

# Is a grouping available?
# (Will return TRUE if an explicit group or a discrete variable with only one
# level existed when add_group() was called.)
has_groups <- function(data) {
  # If no group aesthetic is specified, all values of the group column equal to
  # NO_GROUP. On the other hand, if a group aesthetic is specified, all values
  # are different from NO_GROUP (since they are a result of plyr::id()). NA is
  # returned for 0-row data frames.
  data$group[1L] != NO_GROUP
}
