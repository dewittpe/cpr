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
#' @param method a character string for the regression method
#' @param method.args additional arguments passed to method
#'
#' @rdname generate_cp_formula_data
generate_cp_formula_data <- function(f, data, method, method.args) {

  # part the formula, version with no bspline, no bars
  f_nobsplines <- stats::update(f, paste(". ~ . -", grep("bspline|btensor", attr(stats::terms(f), "term.labels"), value = TRUE)))
  f_nobsplines_nobars <- lme4::nobars(f_nobsplines)

  # get a list of the variables and subset the data
  vars_nobsplines_nobars <- all.vars(lme4::nobars(f_nobsplines_nobars))
  data_nobsplines_nobars <- subset(data, select = vars_nobsplines_nobars)

  if (grepl("geeglm", method)) {
    fit <- do.call(geepack::geeglm, c(method.args, formula = stats::update.formula(f, . ~ 1), data = list(data)))
    data_nobsplines_nobars[[as.character(method.args[["id"]])]] <- unname(fit[["id"]])
  }

  # identify any variables which are factors or characters
  factors <- sapply(data_nobsplines_nobars, function(x) {is.factor(x) | is.character(x)})
  factors <- names(factors[factors])

  # build the data frames
  # extract only the factors and build a model matrix
  if (length(factors)) {
    data_factors_only <-
      data.frame(stats::model.matrix(stats::as.formula(paste("~", paste(factors, collapse = " + "))),
                                     data = data))[, -1]
    new_factors <-
      lapply(factors, function(x) grep(x, names(data_factors_only), value = TRUE))
    new_factors <- paste(do.call(c, new_factors), collapse = " + ")
  } else {
    data_factors_only <- NULL
  }


  data_nobsplines_nobars <-
    subset(data_nobsplines_nobars, select = setdiff(names(data_nobsplines_nobars), factors))

  data_bsplines_bars <-
    subset(data, select = setdiff(intersect(all.vars(lme4::subbars(f)), names(data)), all.vars(f_nobsplines_nobars)))

  # construct the new formula and data set
  if (!is.null(data_factors_only)) {
    f_for_use <-
      stats::update(f, paste(". ~ 0 + . -", paste(factors, collapse = " - "), "+", new_factors))
    data_for_use <-
      cbind(data_nobsplines_nobars, data_bsplines_bars, data_factors_only)
  } else {
    f_for_use <-
      stats::update(f, paste(". ~ 0 + ."))
    data_for_use <-
      cbind(data_nobsplines_nobars, data_bsplines_bars)
  }

  e <- parent.frame()
  e$f_for_use <- f_for_use
  e$data_for_use <- data_for_use
}

factors_characters_in_f <- function(f, data) {
  # part the formula, version with no bspline, no bars
  f_nobsplines <- stats::update(f, paste(". ~ . -", grep("bspline|btensor", attr(stats::terms(f), "term.labels"), value = TRUE)))
  f_nobsplines_nobars <- lme4::nobars(f_nobsplines)

  # get a list of the variables and subset the data
  vars_nobsplines_nobars <- all.vars(lme4::nobars(f_nobsplines_nobars))
  data_nobsplines_nobars <- subset(data, select = vars_nobsplines_nobars)

  # identify any variables which are factors or characters
  factors <- sapply(data_nobsplines_nobars, function(x) {is.factor(x) | is.character(x)})

  return(any(factors))
}
