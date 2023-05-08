.plot_store <- function() {
  .last_plot <- NULL

  list(
    get = function() .last_plot,
    set = function(value) .last_plot <<- value
  )
}
.store <- .plot_store()

#' Set the last plot to be fetched by lastplot()
#'
#' @seealso [last_plot()]
#' @export
#' @keywords internal
set_last_plot <- function(value) .store$set(value)


#' Retrieve the last plot to be modified or created.
#'
#' @seealso [ggsave()]
#' @export
#' @keywords internal
last_plot <- function() .store$get()
