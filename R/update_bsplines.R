#' Update bspline or btensor calls
#'
#' Update \code{cpr_bs} and \code{cpr_bt} objects alone or within \code{cpr_cp}
#' and \code{cpr_cn} objects.
#'
#' @param object an object to update.  
#' @param ... things to update, expected to be \code{iknots}, \code{df},
#' \code{bknots}, or \code{order}.
#' @param evaluate If true evaluate the new call else return the call.
#'
#' @author Peter DeWitt \email{dewittpe@gmail.com}
#'
#' @seealso \code{\link[stats]{update}}, \code{\link{bsplines}},
#' \code{\link{btensor}}
#'
#' @examples
#' ########################### Updating a cpr_bs object ###########################
#' # construct a B-spline basis
#' bmat <- bsplines(seq(1, 10, length = 15), df = 5, order = 3)
#' 
#' # look at the structure of the basis
#' str(bmat)
#' 
#' # change the order
#' str(update_bsplines(bmat, order = 4))
#' 
#' # change the order and the degrees of freedom
#' str(update_bsplines(bmat, df = 12, order = 4))
#' 
#' ########################### Updating a cpr_bt object ###########################
#' # construct a tensor product
#' tpmat <- btensor(list(x1 = seq(0, 1, length = 10), x2 = seq(0, 1, length = 10)),
#'                  df = list(4, 5))
#' 
#' tpmat 
#' 
#' # update the degrees of freedom
#' update_btensor(tpmat, df = list(6, 7))
#' 
#' ####### Updating bsplines or btensor on the right and side of a formula ########
#' 
#' f1 <- y ~ bsplines(x, df = 14) + var1 + var2
#' f2 <- y ~ btensor(x = list(x1, x2), df = list(50, 31), order = list(3, 5))  + var1 + var2
#' 
#' update_bsplines(f1, df = 13, order = 5) 
#' update_btensor(f2, df = list(13, 24), order = list(3, 8))
#' 
#' ########################### Updating a cpr_cp object ###########################
#' data(spdg, package = "cpr")
#' init_cp <- cp(pdg ~ bsplines(day, df = 30) + age + ttm, data = spdg)
#' updt_cp <- update_bsplines(init_cp, df = 5)
#' plot(init_cp, updt_cp, show_spline = TRUE, color = TRUE)
#' 
#' ########################### Updating a cpr_cn object ###########################
#' init_cn <- cn(pdg ~ btensor(list(day, age), df = list(30, 4)) + ttm, data = spdg)
#' updt_cn <- update_btensor(init_cn, df = list(30, 2), order = list(3, 2))
#' 
#' par(mfrow = c(1, 2))
#' plot(init_cn, rgl = FALSE)
#' plot(updt_cn, rgl = FALSE)
#'
#' @export
#' @rdname update_bsplines
update_bsplines <- function(object, ..., evaluate = TRUE) {
  UseMethod("update_bsplines")
}

#' @export
update_bsplines.formula <- function(object, ..., evaluate = TRUE) {
  dots <- as.list(match.call(expand.dots = FALSE))$...
  dots <- dots[!is.na(match(names(dots), c("iknots", "df", "bknots", "order")))] 
  out <- lapply(as.list(object), find_update_b_, dots)
  out <- eval(as.call(out))
  environment(out) <- environment(object)
  out 
}

#' @export
update_bsplines.cpr_bs <- function(object, ..., evaluate = TRUE) { 
  cl <- as.list(attr(object, "call"))
  dots <- match.call(expand.dots = FALSE)$...
  dots <- dots[!is.na(match(names(dots), c("iknots", "df", "bknots", "order")))] 
  
  for(a in names(dots)) { 
    if (!all(c(is.null(cl[[a]]), is.null(dots[[a]])))) {
      cl[[a]] <- dots[[a]]
    }
  }

  if (evaluate) {
    eval(as.call(cl), attr(object, "environment"))
  } else {
    cl
  }
  
}

#' @export
update_bsplines.cpr_cp <- function(object, ..., evaluate = TRUE) {
  dots <- match.call(expand.dots = FALSE)$...
  f <- do.call(update_bsplines.formula, c(list(object = object$call$formula), dots)) 
  x <- stats::update(object, formula = f, evaluate = FALSE) 
  if (evaluate) {
    eval(x, environment(f), parent.frame())
  } else {
    x
  }
}

#' @export
#' @rdname update_bsplines
update_btensor <- function(object, ..., evaluate = TRUE) {
  UseMethod("update_btensor")
}

#' @export
update_btensor.formula <- update_bsplines.formula
#' @export
update_btensor.cpr_cn <- update_bsplines.cpr_cp
#' @export
update_btensor.cpr_bt <- update_bsplines.cpr_bs


find_update_b_ <- function(x, dots) {
  if (is.call(x) && grepl("bsplines|btensor", deparse(x[[1]]))) {
    for(a in names(dots)) { 
      if (!all(c(is.null(x[[a]]), is.null(dots[[a]])))) {
        x[[a]] <- dots[[a]]
      }
    }
    x
  } else if (is.recursive(x)) {
    as.call(lapply(as.list(x), find_update_b_, dots))
  } else {
    x
  }
}
