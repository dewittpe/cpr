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
#' # B-splines and Control Polygons
#'
#' ## B-splines
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
#' function, $\boldsymbol{\xi}$ is a sequence of $l$ interior knots and a total
#' of $2k$ boundary knots, i.e., the cardinality of the knot sequence is
#' $\left\lvert \boldsymbol{\xi} \right\rvert| = 2k + l.$ The value of the $k$
#' boundary knots is arbitrary, but a common choice is to use $k$-fold knots on
#' the boundary:
#'
#' $$ \xi_1 = \xi_2 = \cdots = \xi_k < \xi_{k+1} \leq \cdots \leq \xi_{k + l} =
#' \xi_{k + l + 1} = \cdots \xi_{2k + l}.$$
#'
#' Alternative boundary knots can be used so long as the sequence
#' $\boldsymbol{\xi}$ is non-decreasing.  More on the implications of $k$-fold
#' boundary knots follow in the next section.
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
#' A couple of many important properties of the basis function is that for any value
#' of $x \in \left[\xi_1, \xi_{2k+l}\right]$
#'
#' * $B_{j,k,\boldsymbol{xi}} \left(x\right) \in \left[0, 1\right],$
#' * $ B_{j,k,\boldsymbol{\xi}} \left(x) > 0 \quad \text{for} \quad x \in \left(\xi_{j}, \xi_{j+k}\right),$ and
#' * $\sum_j B_{j,k,\boldsymbol{xi}} \left(x\right) = 1.$
#'
#' Within the cpr package we can generate a basis matrix thusly:
#+ label = "basis_matrix"
x <- seq(0 + 1/5000, 6 - 1/5000, length.out = 5000)
bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
bmat

#'
#' Note: the default order for
{{backtick(bsplines)}}
#' is 4, and the default for the boundary knots is the range of
{{backtick(x) %s% "."}}

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
#' ## Control Polygons
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
#' ## Continuity and Knots
#'
#' All the basis functions are, by definition, right-continuous.  The spline
#' function has at most $k$ continuity conditions.  That, is, for a value of $x$
#' such that $\xi_1 < x < \xi
#'
#' The knots sequence is a representation of the desired smoothness of
#' $f\left(x\right).$  For any point $\xi_1 < x < \xi_{2k + l}} and $\notexists
#' i s.t. x = \xi_i,$ the function $f\left(x\right)$ has $k$ continuity
#' conditions, $(k -1)^{st}-$ through $0^{th}$-order differentiable.  At a knot,
#' there number of continuity conditions decreases for the number of knots at
#' that point.  Overall, the number of continuity conditions at a point $x$ plut
#' the number of knots at $x$ equals $k.$
#'
#' For an example, we will use a $k = 4$ order spline (a cubic spline) to
#' perfectly fit a cubic function.  We'll define the function, first, and second
#' derivatives
f <- function(x) {
  #(x + 2) * (x - 1) * (x - 3)
  x^3 - 2 * x^2 - 5 * x + 6
}

fprime <- function(x) { # first derivatives of f(x)
  3 * x^2 - 4 * x - 5
}

fdoubleprime <- function(x) { # second derivatives of f(x)
  6 * x - 4
}

#'
#' We'll look at the function over
bknots = c(-3, 5)

x     <- seq(-3 + 1/100, 5 - 1/100, length.out = 100)
bmat  <- bsplines(x, bknots = bknots)
theta <- matrix(coef(lm(f(x) ~ bmat + 0)), ncol = 1)

bmatD1 <- bsplineD(x, bknots = bknots, derivative = 1L)
bmatD2 <- bsplineD(x, bknots = bknots, derivative = 2L)

# check that the function f(x) is recovered
all.equal(f(x), as.numeric(bmat %*% theta))

# check that the first derivative is recovered
all.equal(fprime(x), as.numeric(bmatD1 %*% theta))

# check that the second derivative is recovered
all.equal(fdoubleprime(x), as.numeric(bmatD2 %*% theta))

# Plot the results
#+ label = "plot derivatives", fig.width = 7, fig.height = 4
par(mfrow = c(1, 3))
plot(x, f(x), type = "l")
points(x, bmat %*% theta, col = 'blue')

plot(x, fprime(x), type = "l")
points(x, bmatD1 %*% theta, col = 'blue')

plot(x, fdoubleprime(x), type = "l")
points(x, bmatD2 %*% theta, col = 'blue')

#'
#' # Knot Influence
#'
#' ## Spline Spaces
#'
#' Consider two knot sequences $\boldsymbol{\xi}$ and $\boldsymbol{\xi} \cup
#' \boldsymbol{\xi}'.$  Then, for a given polynomial order $k,$ the spline space
#' $\mathbb{S}_{k,\boldsymbol{\xi}} \subset \mathbb{S}_{k,\boldsymbol{\xi} \cup
#' \boldsymbol{\xi}'}$ [@deboor2001,pg135]. Given this relationship between
#' spline spaces, and the convex sums generating spline functions, @boehm1980
#' presented a method for inserting a knots into the knot sequence such that the
#' spline function is unchanged.  Specifically, for
#' $$
#' \boldsymbol{\xi}' = \left\{\left. \xi_{j}' \right|
#' \min\left(\boldsymbol{\xi}\right) < \xi_j <
#' \max\left(\boldsymbol{\xi}\right) \quad \forall j\right\},
#' $$
#' then there exist a $\boldsymbol{\theta}_{\boldsymbol{\xi} \cup
#' \boldsymbol{\xi}'}$ such that
#' $$
#' \boldsymbol{B}_{k,\boldsymbol{\xi}} \left(x\right)\boldsymbol{\theta}_{\boldsymbol{\xi}} =
#' \boldsymbol{B}_{k,\boldsymbol{\xi}\cup\boldsymbol{\xi}'}\left(x\right) \boldsymbol{\theta}_{\boldsymbol{\xi} \cup \boldsymbol{\xi}'}
#' \quad \forall x \in \left[\min\left(\boldsymbol{\xi}\right), \max\left(\boldsymbol{\xi} \right) \right].
#' $$
#'
#' When inserting a singleton $\xi'$ into $\boldsymbol{\xi},$ then
#' $$
#' \boldsymbol{\theta}_{\boldsymbol{\xi} \cup \xi'} = \boldsymbol{W}_{k,
#' \boldsymbol{\xi}}\left(\xi'\right) \boldsymbol{\theta}_{\boldsymbol{\xi}}
#' $$
#' where $\boldsymbol{W}_{k,\boldsymbol{\xi}}\left(\xi'\right)$
#' is a $\left(
#' \left\lvert \boldsymbol{\theta} \right\rvert + 1\right) \times \left\lvert
#' \boldsymbol{\theta} \right\rvert$ lower bi-diagonal matrix
#' $$
#' \boldsymbol{W}_{k,\boldsymbol{\xi}}\left(\xi'\right) =
#' \begin{pmatrix}
#' 1 & 0 & \cdots & 0 \\
#' 1 - \omega_{1, k, \boldsymbol{\xi}}\left(\xi'\right) & \omega_{1, k, \boldsymbol{\xi}} \left(\xi'\right) & \cdots & 0 \\
#' 0 & 1 - \omega_{2, k, \boldsymbol{\xi}}\left(\xi'\right) & \cdots 0 \\
#' \vdots & \vdots & \ddots & \vdots \\
#' 0 & 0 & 1 - \omega_{\left\lvert \boldsymbol{\theta} \right\rvert - 1, k, \boldsymbol{\xi}} \left(\xi'\right) & \omega_{\left\lvert \boldsymbol{\theta} \right\rvert - 1, k, \boldsymbol{\xi}} \left(\xi'\right) \\
#' 0 & 0 & 0 & 1
#' \end{pmatrix},
#' $$
#' with $\omega_{j,k,\boldsymbol{\xi}}\left(x\right)$ as defined above in the
#' de~Boor recursive algorithm. Through recursion we can insert as many knots
#' into $\boldsymbol{\xi}$ without changing the value of the spline function.
#'
#' ## Assessing influence of $\xi_j$
#'
#' Here we derivate a metric for assessing how much influence $\xi_j \in
#' \boldsymbol{\xi}$ has on $\boldsymbol{B}_{k,\boldsymbol{\xi}}
#' \left(x\right)\boldsymbol{\theta}_{\boldsymbol{\xi}}.$ Using the relationship
#' defined by @boehm1980, we can derivate a metric for the influence of $\xi_j$
#' on the spline function.
#'
#' Start with an defined $k, \boldsymbol{\xi},$ and
#' $\boldsymbol{\theta}_{k,\boldsymbol{\xi}}.$ The relationship
#' $$
#' \boldsymbol{\theta}_{\boldsymbol{\xi}} =
#' \boldsymbol{W}_{k,\boldsymbol{\xi}\backslash\xi_{j}}\left(\xi_{j}\right)
#' \boldsymbol{\theta}_{\boldsymbol{\xi}\backslash\xi_{j}}
#' $$
#' holds if $\xi_j$ has zero influence.  However, in practice, we would expect
#' that the relationship is
#' $$
#' \boldsymbol{\theta}_{\boldsymbol{\xi}} =
#' \boldsymbol{W}_{k,\boldsymbol{\xi}\backslash\xi_{j}}\left(\xi_{j}\right)
#' \boldsymbol{\theta}_{\boldsymbol{\xi}\backslash\xi_{j}} + \boldsymbol{d}
#' $$
#' for a set of deviations $\boldsymbol{d}$ which would be equal to
#' $\boldsymbol{0}$ if $\xi_j$ has no influence on the spline.
#'
#' We can estimate $\boldsymbol{d}$ via least squares.  To simplify the notation
#' in the following we drop some of the subscripts and parenthesis, that is, let
#' $\boldsymbol{W} =
#' \boldsymbol{W}_{k,\boldsymbol{\xi}\backslash\xi_{j}}\left(\xi_{j}\right).$
#'
#' $$
#' \begin{align}
#' \boldsymbol{d} &= \boldsymbol{\theta}_{\boldsymbol{\xi}} - \boldsymbol{W} \boldsymbol{\theta}_{\boldsymbol{\xi}\backslash\xi_{j}} \\
#'                &= \boldsymbol{\theta}_{\boldsymbol{\xi}} - \boldsymbol{W} \left(\boldsymbol{W}^{T}\boldsymbol{W}\right)^{-1}\boldsymbol{W}\boldsymbol{\theta}_{\boldsymbol{\xi}} \\
#'                &= \left(\boldsymbol{I} - \boldsymbol{W} \left(\boldsymbol{W}^{T}\boldsymbol{W}\right)^{-1}\boldsymbol{W}\right)\boldsymbol{\theta}_{\boldsymbol{\xi}} \\
#' \end{align}
#' $$
#'
#'
#'
#' # References
#'<div id="refs"></div>
#'
#' # Session Info
#+ label = "sessioninfo"
sessionInfo()

# /* ---------------------------- END OF FILE ------------------------------- */
