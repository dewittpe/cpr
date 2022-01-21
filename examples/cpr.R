###############################################################################
# Example 1: find a model for log10(pdg) = f(day) from the spdg data set
\dontrun{
# need the lme4 package to fit a mixed effect model
# library(lme4)

# construct the initial control polygon.  Forth order spline with fifty internal
# knots.  Remember degrees of freedom equal the polynomial order plus number of
# internal knots.
init_cp <- cp(log10(pdg) ~ bsplines(day, df = 54) + (1|id),
              data = spdg, method = lme4::lmer)
cpr_run <- cpr(init_cp)
plot(cpr_run)
plot(cpr_run, type = "rmse", to = 10)

# preferable model is in index 4
preferable_cp <- cpr_run[[4]]

}

###############################################################################
# Example 2: logistic regression
# simulate a binary response Pr(y = 1 | x) = p(x)
p <- function(x) { 0.65 * sin(x * 0.70) + 0.3 * cos(x * 4.2) }

set.seed(42)
x <- runif(2500, 0.00, 4.5)
sim_data <- data.frame(x = x, y = rbinom(2500, 1, p(x)))

# Define the initial control polygon
init_cp <- cp(formula = y ~ bsplines(x, df = 54, bknots = c(0, 4.5)),
              family  = binomial(),
              method  = glm,
              data    = sim_data)

# run CPR, preferable model is in index 7
cpr_run <- cpr(init_cp)
plot(cpr_run, color = TRUE, type = "rmse", to = 15)

# plot the fitted spline and the true p(x)
pred_data <-
  dplyr::mutate(sim_data,
                pred_select_p = qwraps2::invlogit(predict(cpr_run[[7]], newdata = sim_data)$pred))

ggplot2::ggplot(pred_data) +
ggplot2::theme_bw() +
ggplot2::aes(x = x) +
ggplot2::geom_point(mapping = ggplot2::aes(y = y), alpha = 0.2) +
ggplot2::geom_line(mapping = ggplot2::aes(y = pred_select_p, color = "pred_select_p")) +
ggplot2::stat_function(fun = p, mapping = ggplot2::aes(color = 'p(x)'))

###############################################################################
