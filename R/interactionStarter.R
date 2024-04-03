#' @export
. <- structure(NA, class = "InteractionStarter")

#' @export
print.InteractionStarter <- function(x, ...) {
  interact()
}
