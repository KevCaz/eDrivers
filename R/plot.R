#' Plot methods for objects of class `eDrivers*`
#'
#' Plots the outputs of an `eDrivers*` object call
#'
#' @param x An object of class `eDrivers*`.
#' @param ... ignored.
#'
#' @return The plot of the `eDrivers*` object.
#'
#' @name plot.eDrivers
NULL

#' @rdname plot.eDrivers
#' @method plot eDrivers
#' @export
plot.eDrivers <- function(x, ...) {
  plot(x$Data)
}

#' @rdname plot.eDrivers
#' @method plot eDriversBrick
#' @export
plot.eDriversBrick <- function(x, ...) {
  plot(x$Data)
}
