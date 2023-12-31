#' New Knots for CPs and CNs in CPR and CNR
#'
#' Non-exported function, \code{newknots} are used in the \code{\link{cpr}} and
#' \code{\link{cnr}} calls.  Used to create a new control polygon or control net
#' from with different internal knots.
#'
#' Think of this function as an analogue to the \code{\link{stats}{update}}
#' calls.  Where \code{\link{stats}{update}} will modify a \code{call}, the
#' \code{newknots} will update just the \code{iknots} argument of a
#' \code{bsplines} or \code{btensor} call within the \code{formula} argument of
#' a \code{\link{cp}} or \code{\link{cn}} call.
#'
#' @param form a \code{formula}
#' @param nk   numeric vector, or a list of numeric vectors, to be used in a
#' \code{\link{bsplines}} or \code{\link{btensor}} call, respectively.
#'
#' @return Expected use is within the \code{cpr} and \code{cnr} calls.  The
#' return object a formula to define a control polygon/net
#' with different knots than then ones found within \code{form}.
#'
#' @seealso \code{\link{update_bsplines}} for a more generic tool for the end
#' user.
#'
#' @examples
#'
#' cp0 <- cp(log(pdg) ~ bsplines(day, iknots = c(-.25, 0, 0.25), bknots = c(-1, 1)), data = spdg)
#' cpr:::newknots(cp0$call$formula, c(-0.85, 0, 0.25, 0.3))
#'
newknots <- function(form, nk) {
  rr <- function(x, nk) {
    if(is.call(x) && grepl("bsplines|btensor", deparse(x[[1]]))) {
      x$df <- NULL
      x$iknots <- nk
      x
    } else if (is.recursive(x)) {
      as.call(lapply(as.list(x), rr, nk))
    } else {
      x
    }
  }

  z <- lapply(as.list(form), rr, nk)
  z <- eval(as.call(z))
  environment(z) <- environment(form)
  z
}
