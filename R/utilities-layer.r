# This needs to be less than 1, to distinguish it from "regular" return values
# of plyr::id() used by add_group()
NO_GROUP <- -1L

# Ensure that the data frame contains a grouping variable.
#
# If the \code{group} variable is not present, then a new group
# variable is generated from the interaction of all discrete (factor or
# character) vectors, excluding \code{label}. The special value \code{NO_GROUP}
# is used for all observations if no discrete variables exist.
#
# @param data.frame
# @value data.frame with group variable
# @keyword internal
# @seealso has_groups
add_group <- function(data) {
  if (empty(data)) return(data)

  if (is.null(data$group)) {
    disc <- vapply(data, is.discrete, logical(1))
    disc[names(disc) == "label"] <- FALSE

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

order_groups <- function(data) {
  if (is.null(data$order)) return(data)

  data[order(data$order), ]
}

# Is a grouping available?
has_groups <- function(data) {
  # If no group aesthetic is specified, all values of the group column equal to
  # NO_GROUP. On the other hand, if a group aesthetic is specified, all values
  # are different from NO_GROUP (since they are a result of plyr::id()). NA is
  # returned for 0-row data frames.
  data$group[1L] != NO_GROUP
}
