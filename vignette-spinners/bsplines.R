#'---
#'title: "B-Splines and Control Polygons"
#'author: "Peter E. DeWitt"
#'output:
#'  rmarkdown::html_vignette:
#'    toc: true
#'    number_sections: true
#'bibliography: references.bib
#'vignette: >
#'  %\VignetteIndexEntry{B-Splines and Control Polygons}
#'  %\VignetteEngine{knitr::rmarkdown}
#'  %\VignetteEncoding{UTF-8}
#'---
#'
#' <style type="text/css">
#' window.MathJax = {
#'    tex: {
#'      tags: 'ams'
#'    }
#' };
#' </style>
#'
#+ label = "setup", include = FALSE
library(qwraps2)
options(qwraps2_markup = "markdown")
knitr::opts_chunk$set(collapse = TRUE)
#'
# /*
devtools::load_all()
# */
library(cpr)
packageVersion("cpr")
#'
#' The term "spline" is likely derived from shipwright or draftsmen splines,
#' thin wood strips, held in place by weights, used to define curves.  These
#' splines naturally minimize strain energy and the use of additional weights at
#' strategic locations on the spline are needed to achieve specific curvatures.
#' Cubic B-splines are not dissimilar.
#'
#' Splines are piecewise polynomial curves that are differentiable up to a
#' prescribed order. B-splines are based on a _basis_ of polynomial functions.
#'
#' Definitions and notation for uni-variable and multi-variable B-splines and
#' the associated control polygons and control nets are presented in the
#' following.  Two very good references for splines are @deboor2001 and
#' @prautzsch2002 if you wish to dig into the details.
#'
#' # Uni-variable B-splines and Control Polygons
#'
#' A curve defined by $f\left(x\right)$ is a spline of order $k$ (degree $k
#' -1$), with knots $\xi_1, \xi_2, \ldots, \xi_m$ where $\xi_i \leq x_{i+1}$ and
#' $\xi_{i} < \xi_{i + k} \forall i$ if $f\left(x\right)$ is $k-r-1$ times
#' differentiable at any $r$-fold knot, and $f\left(x\right)$ is a polynomial of
#' order $\leq k$ over each interval $x \in \left[\xi_i, \xi_{i+1}\right]$ for
#' $i = 0, \ldots, m - 1.$
#'
#' In particular, B-splines, are defined as an affine combination:
#'
#' \begin{equation}
#'   f \left( x \right) =
#'   \sum_{j} \theta_j B_{j,k,\boldsymbol{\xi}}\left(x\right) =
#'   \boldsymbol{B}_{k, \boldsymbol{\xi}} \left(x\right) \boldsymbol{\theta}_{\boldsymbol{\xi}}
#'   \label{eq:f}
#' \end{equation}
#'
#' where $B_{j,k,\boldsymbol{\xi}}\left(x\right)$ is the $j^{th}$ basis spline
#' function, $\boldsymbol{\xi}$ is a sequence of knots with $k$-fold boundary knots
#' and $l \geq 0$ interior knots, i.e.,
#'
#' $$ \xi_1 = \xi_2 = \cdots = \xi_k < \xi_{k+1} \leq \cdots \leq \xi_{k + l} =
#' \xi_{k + l + 1} = \cdots \xi_{2k + l}.$$
#'
#' The $j^{th}$ B-spline is defined as:
#'
#' \begin{equation}
#' B_{j,k,\boldsymbol{\xi}}\left(x\right) =
#'  \omega_{j,k,\boldsymbol{\xi}} \left(x\right)
#'  B_{j,k-1,\boldsymbol{\xi}}\left(x\right) +
#'  \left(1 - \omega_{j+1,k,\boldsymbol{\xi}} \right)
#'  B_{j+1,k-1,\boldsymbol{\xi}}\left(x\right),
#' \end{equation}
#' where
#' \begin{equation}
#' B_{j,k,\boldsymbol{\xi}}\left(x\right) = 0 \quad \text{for} \quad x \notin
#' \left[\xi_j, \xi_{j+k} \right), \quad \quad
#' B_{j,1,\boldsymbol{\xi}}\left(x\right) = \begin{cases}
#' 1 & x \in \left[\xi_j, \xi_{j+k}\right) \\ 0 & \text{otherwise},
#' \end{cases}
#' \end{equation}
#' and
#' \begin{equation}
#' \omega_{j,k,\boldsymbol{\xi}}\left(x\right) = \begin{cases}
#' 0 & x \leq \xi_j \\ \frac{x-\xi_j}{\xi_{j+k-1} - \xi_{j}} & \xi_j < x < \xi_{j+k-1} \\
#' 1 & \xi_{j+k-1} \leq x.
#' \end{cases}
#' \end{equation}
#'
#' For a set of observations, $\boldsymbol{x} = x_1, \ldots, x_n,$ the basis
#' functions defined above generalize to a matrix:
#'
#' \begin{equation}
#'   \boldsymbol{B}_{k, \boldsymbol{\xi}} \left(\boldsymbol{x}\right) =
#'   \begin{pmatrix}
#'     B_{1, k, \boldsymbol{\xi}} \left(x_1\right) &
#'     B_{2, k, \boldsymbol{\xi}} \left(x_1\right) &
#'     \ldots &
#'     B_{k + l, k, \boldsymbol{\xi}} \left(x_1\right) \\
#'     \vdots & \vdots & \ddots & \vdots \\
#'     B_{1, k, \boldsymbol{\xi}} \left(x_n\right) &
#'     B_{2, k, \boldsymbol{\xi}} \left(x_n\right) &
#'     \ldots &
#'     B_{k + l, k, \boldsymbol{\xi}} \left(x_n\right).
#'   \end{pmatrix}
#' \end{equation}
#'
#' Within the cpr package we can generate a basis matrix thusly:
#+ label = "basis_matrix"
right_continuous_seq <- function(from, to, ..., right_nudge = sqrt(.Machine$double.eps)) {
  x <- seq(from = from, to = to - right_nudge, ...)
  attr(x, "from") = from
  attr(x, "to") = to
  x
}

x <- right_continuous_seq(0, 6, length.out = 5000)
bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
bmat

#'
#' Note: the default order for
{{backtick(bsplines)}}
#' is 4, and the default for the boundary knots is the range of
{{backtick(x) %s% "."}}

#'
#' A couple of many important properties of the basis function is that for any value
#' of $x \in \left[\xi_1, \xi_{2k+l}\right]$
#'
#' * $B_{j,k,\boldsymbol{xi}} \left(x\right) \in \left[0, 1\right],$ and
#' * $\sum_j B_{j,k,\boldsymbol{xi}} \left(x\right) = 1.$
#'
#+ label = "row sums to 1"
all(bmat >= 0)
all(bmat <= 1)
all.equal(rowSums(bmat), rep(1, nrow(bmat)))

#'
#' We can quickly view the plot of each of these spline functions as well.
#+ label = "plot bmat", fig.width = 7, fig.height = 4
plot(bmat, show_xi = TRUE, show_x = TRUE)

#'
#' The spline $f\left(\boldsymbol{x}\right) = \boldsymbol{B}_{k,\boldsymbol{\xi}}\left(\boldsymbol{x}\right)$
#' is a convex sum of the coefficients $\boldsymbol{\theta}_{\boldsymbol{\xi}}.$
#' A meaningful geometric
#' relationship between $\boldsymbol{\xi}$ and
#' $\boldsymbol{\theta}_{\boldsymbol{\xi}}$ exist in the form of a
#' __control polygon__,
#' $CP_{k,\boldsymbol{\xi},\boldsymbol{\theta}_{\boldsymbol{\xi}}},$ a strong
#' convex hull for
#' $\boldsymbol{B}_{k,\boldsymbol{\xi}}\left(\boldsymbol{x}\right)
#' \boldsymbol{\theta}_{\boldsymbol{\xi}}.$
#'
#' \begin{equation}
#' CP_{k,\boldsymbol{\xi},\boldsymbol{\theta}_{\xi}} =
#' \left\{ \left( \xi_{j}^{*}, \theta_{j,\boldsymbol{\xi}} \right) \right
#' \}_{j=1}^{\left\lvert\boldsymbol{\theta}_{\boldsymbol{\xi}}\right\rvert};
#' \quad \quad
#' \xi_{j}^{*} = \frac{1}{k-1} \sum_{i = 1}^{k-1} \xi_{j + i}.
#' \end{equation}
#'
#' $CP_{k,\boldsymbol{\xi},\boldsymbol{\theta}_{\boldsymbol{\xi}}}$ is a
#' sequence of $\left\lvert \boldsymbol{\theta}_{\boldsymbol{\xi}} \right\rvert
#' = k + l$ control vertices.  The control polygon can be thought of as a
#' piecewise linear function approximating the spline function.  Changes in
#' convexity and other subtle characteristics of the spline function are
#' exaggerated by the control polygon.
#'
#' For example, using the basis matrix defined above and the following
#' coefficients we can easily define a spline function and control polygon.
#+ label = "define theta"
theta <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
cp(bmat, theta)

#'
#' Plotting the control polygon and the corresponding spline:
#'
#+ label = "plot cp", fig.width = 7, fig.height = 4
plot(cp(bmat, theta), show_spline = TRUE)

#'
#' ## Right-Continuity and Implimentation
#'
#' By definition, B-splines are right continuous.  This has several

#'
#' ## Derivatives
#'
# derivatives are as expected

f0 <- function(x) {
  #(x + 2) * (x - 1) * (x - 3)
  x^3 - 2 * x^2 - 5 * x + 6
}
f1 <- function(x) {
  3 * x^2 - 4 * x - 5
}
f2 <- function(x) {
  6 * x - 4
}


#x <- seq(-3, 5, length = 100)
x <- sort(runif(n = 100, min = -3, max = 5))
bknots = c(-3, 5)
bmat <- bsplines(x, bknots = bknots)
theta <- coef(lm(f0(x) ~ bsplines(x, bknots = bknots) + 0) )

baseD1 <- splines::spline.des(c(-3, -3, -3, -3, 5, 5, 5, 5), x, derivs = rep(1, length(x)))$design
cprD1 <- bsplineD(x, bknots = bknots, derivative = 1L)


baseD2 <- splines::spline.des(c(-3, -3, -3, -3, 5, 5, 5, 5), x, derivs = rep(2, length(x)))$design
cprD2 <- bsplineD(x, bknots = bknots, derivative = 2L)

# verify that I can get cubic correct
#stopifnot(isTRUE(all.equal(f0(x), as.numeric(bmat %*% theta))))

# verify that I can get the first derivative correct -- this is different from
# base R in that, by my construction, the derivative at the boundardy knots
# should be NA, as the k-fold knots should result in no differentiability at
# these points.
#stopifnot(isTRUE(all.equal(f1(x), as.numeric(cprD1 %*% theta))))

# verify that I can get the second derivative correct
#stopifnot(isTRUE(all.equal(f2(x), as.numeric(cprD2 %*% theta))))

par(mfrow = c(1, 3))
plot(x, f0(x), type = "l")
points(x, bmat %*% theta, pch = 2, col = 'blue')

plot(x, f1(x), type = "l")
points(x, baseD1 %*% theta, pch = 16, col = 'red', cex = 0.7)
points(x, cprD1 %*% theta, pch = 2, col = 'blue')

plot(x, f2(x), type = "l")
points(x, baseD2 %*% theta, pch = 16, col = 'red', cex = 0.7)
points(x, cprD2 %*% theta, pch = 2, col = 'blue')


#'
#'
#' # References
#'<div id="refs"></div>
#'
#' # Session Info
#+ label = "sessioninfo"
sessionInfo()

# /* ---------------------------- END OF FILE ------------------------------- */
