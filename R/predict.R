#' Model Prediction
#'
#' Model prediction for \code{cpr_cp} and \code{cpr_cn} objects.
#'
#' @author Peter DeWitt \email{dewittpe@gmail.com}
#'
#' @param object a \code{cpr_cp} or \code{cpr_cn} object
#' @param newdata a \code{data.frame}
#' @param ... passed to \code{\link[stats]{predict}} from the \code{stats}
#' package.
#'
#' @export
predict.cpr_cp <- function(object, newdata, ...) {
  f_for_use <- data_for_use <- NULL
  generate_cp_formula_data(updatebsplines(stats::formula(object), object$iknots, object$bknots, object$order), newdata)
  XMAT <- stats::model.matrix(lme4::nobars(f_for_use)[-2], data_for_use)
  data.frame(pred = as.numeric(XMAT %*% object$coefficients),
             se   = apply(XMAT, 1, function(x, sg) {sqrt(matrix(x, nrow = 1) %*% sg %*% matrix(x, ncol = 1))}, sg = object$vcov))
}

#' @export
predict.cpr_cn <- function(object, newdata, ...) {

  iks <- lapply(object$bspline_list, attr, which = "iknots")
  bks <- lapply(object$bspline_list, attr, which = "bknots")
  ods <- lapply(object$bspline_list, attr, which = "order")

  f_for_use <- data_for_use <- NULL
  generate_cp_formula_data(updatebsplines(stats::formula(object), iks, bks, ods), newdata)
  XMAT <- stats::model.matrix(lme4::nobars(f_for_use)[-2], data_for_use)
  data.frame(pred = as.numeric(XMAT %*% object$coefficients),
             se   = apply(XMAT, 1, function(x, sg) {sqrt(matrix(x, nrow = 1) %*% sg %*% matrix(x, ncol = 1))}, sg = object$vcov))
}

updatebsplines <- function(form, nik, nbk, nord) {
  rr <- function(x, nik, nbk, nord) {
    if(is.call(x) && grepl("bsplines|btensor", deparse(x[[1]]))) {
      x$df <- NULL
      x$iknots <- nik
      x$bknots <- nbk
      x$order  <- nord
      x
    } else if (is.recursive(x)) {
      as.call(lapply(as.list(x), rr, nik, nbk, nord))
    } else {
      x
    }
  }

  z <- lapply(as.list(form), rr, nik, nbk, nord)
  z <- eval(as.call(z))
  environment(z) <- environment(form)
  z
}

