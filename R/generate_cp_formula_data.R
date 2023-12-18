#' Generate Control Polygon Formula and Data
#'
#' Construct a \code{data.frame} and \code{formula} to be passed to the
#' regression modeling tool to generate a control polygon.
#'
#' This function is expected to be called from within the \code{cp}
#' function and is not expected to be called by the end user directly.
#'
#' \code{generate_cp_data} exists because of the need to build what could be
#' considered a varying means model.  \code{y ~ bsplines(x1) + x2} will generate
#' a rank deficient model matrix---the rows of the bspline basis matrix sum to
#' one with is perfectly collinear with the implicit intercept term.  Specifying
#' a formula \code{y ~ bsplines(x1) + x2 - 1} would work if \code{x2} is a
#' continuous variable.  If, however, \code{x2} is a factor, or coerced to a
#' factor, then the model matrix will again be rank deficient as a column for
#' all levels of the factor will be generated.  We need to replace the intercept
#' column of the model matrix with the bspline.  This also needs to be done for
#' a variety of possible model calls, \code{\link[stats]{lm}},
#' \code{\link[lme4]{lmer}}, etc.
#'
#' By returning an explicit \code{formula} and \code{data.frame} for use in the
#' fit, we hope to reduce memory use and increase the speed of the cpr method.
#'
#' We need to know the \code{method} and \code{method.args} to build the data
#' set.  For example, for a \code{\link[geepack]{geeglm}} the \code{id} variable
#' is needed in the data set and is part of the \code{method.args} not the
#' \code{formula}.
#'
#' @param f a formula
#' @param data the data set containing the variables in the formula
#' @param formula_only if TRUE then only generate the formula, when FALSE, then
#' generate and assign the data set too.
#' @param envir the environment the generated formula and data set will be
#' assigned too.
#'
#' @return TRUE, invisibly.  The return isn't needed as the assignment happens
#' within the call.
#'
#' @examples
#'
#' e <- new.env()
#' with(e, {
#'   data <-
#'     data.frame(
#'                  x1 = runif(20)
#'                , x2 = runif(20)
#'                , x3 = runif(20)
#'                , xf = factor(rep(c("l1","l2","l3","l4"), each = 5))
#'                , xc = rep(c("c1","c2","c3","c4", "c5"), each = 4)
#'                , pid = gl(n = 2, k = 10)
#'                , pid2 = rep(1:2, each = 10)
#'     )
#'
#'   f <- ~ bsplines(x1, bknots = c(0,1)) + x2 + xf + xc + (x3 | pid2)
#'
#'   cpr:::generate_cp_formula_data(f, data)
#'
#'   stopifnot(isTRUE(
#'     all.equal(
#'               f_for_use
#'               ,
#'               . ~ bsplines(x1, bknots = c(0, 1)) + x2 + (x3 | pid2) + xfl2 + xfl3 + xfl4 + xcc2 + xcc3 + xcc4 + xcc5 - 1
#'               )
#'   ))
#'
#'   stopifnot(isTRUE(identical(
#'     names(data_for_use)
#'     ,
#'     c("x1", "x2", "x3", "pid", "pid2", "xfl2", "xfl3", "xfl4", "xcc2", "xcc3", "xcc4", "xcc5")
#'   )))
#'
#' })
#' @rdname generate_cp_formula_data
generate_cp_formula_data <- function(f, data, formula_only = FALSE, envir = parent.frame()) {

  # get a formula without any bspline, btensor, or bars
  term_labels <- attr(stats::terms(f), "term.labels")
  bspline_btensor_term <- grep("bsplines|btensor", term_labels)
  bar_term <- grep("\\|", term_labels)

  # get variables not in the bsplines or btensor, not in (with) bars
  if (
      (length(term_labels) == 1L) |
      (length(term_labels) == 2L & length(bar_term) == 1L)
     ) {
    f1 <- f
  } else if (length(bar_term) > 0) {
    f1 <- stats::drop.terms(stats::terms(f), c(bspline_btensor_term, bar_term))
  } else {
    f1 <- stats::drop.terms(stats::terms(f), c(bspline_btensor_term))
  }

  # check if any of the variables in f1 are factors or characters
  fcvars <- sapply(all.vars(f1), function(x) {is.factor(data[[x]]) | is.character(data[[x]])})

  if (!any(fcvars)) {
    f_for_use <- stats::update.formula(f, . ~ 0 + .)

    if (!formula_only) {
      data_for_use <- data
    }

  } else {
    fcvars <- names(fcvars)[fcvars]
    fcf <- stats::as.formula(paste("~", paste(fcvars, collapse = "+")))
    fcmm <- stats::model.matrix(fcf, data)[, -1]


    f1 <- stats::as.formula(paste(" . ~ 0 + . - ",
                           paste(fcvars, collapse = "-"), "+",
                           paste(colnames(fcmm), collapse = "+")))

    f_for_use <- stats::update.formula(f, f1)

    if (!formula_only) {
      data_for_use <- cbind(data[!(names(data) %in% fcvars)], fcmm)
    }

  }

  assign("f_for_use", f_for_use, envir = envir)

  if (!formula_only) {
    assign("data_for_use", data_for_use, envir = envir)
  }

  invisible(TRUE)
}
