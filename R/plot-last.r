.plot_store <- function() {
  .last_plot <- NULL

  list(
    get = function() .last_plot,
    set = function(value) .last_plot <<- value
  )
}
.store <- .plot_store()

# Set last plot created or modified
set_last_plot <- function(value) .store$set(value)


#' Retrieve the last plot to be modified or created.
#'
#' @seealso \code{\link{ggsave}}
#' @export
last_plot <- function() .store$get()
