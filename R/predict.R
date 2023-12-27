#' Model Prediction
#'
#' Model prediction for \code{cpr_cp} and \code{cpr_cn} objects.
#'
#' @param object a \code{cpr_cp} or \code{cpr_cn} object
#' @param ... passed to \code{\link[stats]{predict}}
#'
#' @return the same as you would get from calling \code{\link[stats]{predict}}
#' on the \code{object$fit}.
#'
#' @examples
#'
#' acp <- cp(log10(pdg) ~ bsplines(age, df = 12, bknots = c(45, 53))
#'            , data = spdg
#'           , keep_fit = TRUE)
#' acp_pred0 <- predict(acp$fit, se.fit = TRUE)
#' acp_pred <- predict(acp, se.fit = TRUE)
#' all.equal(acp_pred0, acp_pred)
#'
#' @export
predict.cpr_cp <- function(object, ...) {

  if (is.null(object[["fit"]])) {
    stop(sprintf("%s[['fit']] is NULL.  Set `keep_fit = TRUE` in cp call and try again.", deparse(substitute(object))))
  }

  cl <- as.list(match.call())[-1]

  dots <- list(...)

  if ("newdata" %in% names(cl)) {
    f_for_use <- data_for_use <- NULL

    do.call(generate_cp_formula_data,
            list(
                   f = as.list(object$call)[["formula"]]
                 , data = dots[["newdata"]]
                 , formula_only = FALSE
            )
    )

    cl[["object"]] <- object[["fit"]]
    cl[["newdata"]] <- data_for_use

  } else {
    cl[["object"]] <- object[["fit"]]
  }

  do.call(what = stats::predict, args = cl)
}

#' @export
predict.cpr_cn <- predict.cpr_cp
