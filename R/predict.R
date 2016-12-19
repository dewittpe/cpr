#' Model Prediction
#'
#' Model prediction for \code{cpr_cp} and \code{cpr_cn} objects.
#'
#' @param object a \code{cpr_cp} or \code{cpr_cn} object
#' @param ... 
#'
#' @export
predict.cpr_cp <- function(object, newdata, ...) {
  generate_cp_formula_data(updatebsplines(formula(object), object$iknots, object$bknots, object$order), newdata)
  XMAT <- model.matrix(lme4::nobars(f_for_use)[-2], data_for_use)
  dplyr::data_frame(pred = as.numeric(XMAT %*% object$coefficients),
                    se   = apply(XMAT, 1, function(x, sg) {sqrt(matrix(x, nrow = 1) %*% sg %*% matrix(x, ncol = 1))}, sg = object$vcov)
                    ) 
}

#' @export
predict.cpr_cn <- function(object, ...) {
  generate_cp_formula_data(updatebsplines(formula(object), object$iknots, object$bknots, object$order), newdata)
  XMAT <- model.matrix(lme4::nobars(f_for_use)[-2], data_for_use)
  dplyr::data_frame(pred = as.numeric(XMAT %*% object$coefficients),
                    se   = apply(XMAT, 1, function(x, sg) {sqrt(matrix(x, nrow = 1) %*% sg %*% matrix(x, ncol = 1))}, sg = object$vcov)
                    ) 
}

updatebsplines <- function(form, nik, nbk, nord) { 
  rr <- function(x, nk) {
    if(is.call(x) && grepl("bsplines|btensor", deparse(x[[1]]))) {
      x$df <- NULL
      x$iknots <- nik
      x$bknots <- nbk
      x$order  <- nord
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

