library(devtools)
document()
#library(ggplot2)
load_all()

ggplot2::ggplot() + plot(this_basis, ggplot2 = T) 


# Testing the basis
xvec <- seq(-2 * pi, 2 * pi, length = 10000)
test_data <- data.frame(x = xvec, y = cos(xvec))

iknots_ <- runif(rpois(1, lambda = 4), -1.9 * pi, 1.9 * pi)
iknots_

this_basis <- with(test_data, cpr::bsplines(x, iknots = iknots_, bknots = range(x)))
this_basis <- with(test_data, cpr::bsplines(x, iknots = iknots_))

plot(this_basis, ggplot2 = F)


fit1 <- lm(y ~ cpr::bs(x, iknots = iknots_, bknots = range(x)) + 0, data = test_data)
fit2 <- lm(y ~ splines::bs(x, knots = iknots_, intercept = TRUE) + 0, data = test_data)

ggplot() + 
aes(x = x, y = y) + 
geom_line(data = test_data) + 
geom_line(data = data.frame(x = rep(xvec, 2),
                          y = c(unname(fitted(fit1)), unname(fitted(fit2))),
                          fit = gl(n = 2, k = length(xvec), labels = c("fit1", "fit2"))),
        aes(color = fit))
        

all.equal(fitted(fit1), fitted(fit2))


## cp test: building the control polygon
load_all()

is.call(extract.cpr_bspline(y ~ age + bsplines(x, iknots = c(2, 4)) + sex + 0))
eval(extract.cpr_bspline(y ~ age + bsplines(x, iknots = c(2, 4)) + sex + 0)

?as.call

xvec <- seq(-2 * pi, 2 * pi, length = 10000)
test_data <- data.frame(x = xvec, y = cos(xvec), age = runif(length(xvec)), sex = rbinom(length(xvec), 1, 0.5))
cp(y ~ age + bs(x) + sex, data = test_data)
cp(y ~ age + bsplines(x) + sex, data = test_data)
cp(y ~ age + bsplines(x) + sex + 0, data = test_data)
cp(y ~ cpr::bsplines(x) + age + sex + 0, data = test_data)
cp(y ~ age + cpr::bsplines(x, iknots = c(2, 4)) + sex + 0, data = test_data)
cp(y ~ age * cpr::bsplines(x, iknots = c(2, 4)) + sex + 0, data = test_data)


terms( y ~ x + x1 * x - 1)
fterms <- terms(y ~ age * cpr::bsplines(x) + sex + 0)

     grepl("bsplines", attr(fterms, "term.labels"))
