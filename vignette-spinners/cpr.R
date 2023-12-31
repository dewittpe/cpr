#'---
#'title: "Control Polygon Reduction"
#'author: "Peter E. DeWitt"
#'output:
#'  rmarkdown::html_vignette:
#'    toc: true
#'    number_sections: false
#'bibliography: references.bib
#'vignette: >
#'  %\VignetteIndexEntry{Control Polygon Reduction}
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
#' The purpose of this vignette is to illustrate how the Control Polygon
#' Reduction (CPR) method can be used to select a set of knots defining B-spline
#' to get a low degree of freedom and smooth fit to data.  We start with a
#' primer on B-splines and control polygons then the development and use of CPR.
#'
#' # B-splines and Control Polygons
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
#' ## B-splines
#'
#' A curve defined by $f\left(x\right)$ is a spline of order $k$ (degree $k
#' -1$), with knots $\xi_1, \xi_2, \ldots, \xi_m$ where $\xi_j \leq x_{j+1}$ and
#' $\xi_{j} < \xi_{j + k} \forall j$ if $f\left(x\right)$ is $k-r-1$ times
#' differentiable at any $r$-fold knot, and $f\left(x\right)$ is a polynomial of
#' order $\leq k$ over each interval $x \in \left[\xi_j, \xi_{j+1}\right]$ for
#' $j = 0, \ldots, m - 1.$
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
#' $\left\lvert \boldsymbol{\xi} \right\rvert = 2k + l.$ The value of the $k$
#' boundary knots is arbitrary, but a common choice is to use $k$-fold knots on
#' the boundary:
#'
#' $$ \xi_1 = \xi_2 = \cdots = \xi_k < \xi_{k+1} \leq \cdots \leq \xi_{k + l} =
#' \xi_{k + l + 1} = \cdots = \xi_{2k + l}.$$
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
#' Within the cpr package we can generate a basis matrix thusly:
#+ label = "basis_matrix"
x <- seq(0, 5.9999, length.out = 5000)
bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
bmat

#'
#' Note: the default order for
{{backtick(bsplines)}}
#' is 4, and the default for the boundary knots is the range of
{{backtick(x) %s% "."}}
#' However, relying on the default boundary knots can lead to unexpected
#' behavior as, by definition the splines on the $k$-fold upper boundary is 0.
#'
#' We can quickly view the plot of each of these spline functions as well.
#+ label = "plot_bmat", fig.width = 7, fig.height = 4
plot(bmat, show_xi = TRUE, show_x = TRUE)

#'
#' A few of many important properties of the basis functions:
#'
#' $$B_{j,k,\boldsymbol{xi}} \left(x\right) \in \left[0, 1\right], \forall x \in \mathbb{R};$$
all(bmat >= 0)
all(bmat <= 1)

#'
#' $$B_{j,k,\boldsymbol{\xi}} \left(x\right) > 0 \quad \text{for} \quad x \in \left(\xi_{j}, \xi_{j+k}\right);$$
#'
#' and
#'
#' $$\sum_j B_{j,k,\boldsymbol{xi}} \left(x\right) = \begin{cases} 1 & x \in
#' \left[\xi_1, \xi_{2k+l}\right) \\ 0 & otherwise. \end{cases}$$
#'
all.equal(rowSums(bmat), rep(1, nrow(bmat)))

#'
#' ### cpr::bsplines vs splines::bs
#'
#' Part of the base R distribution is the splines package which build B-splines
#' by calling
{{ backtick(bs) %s% "."}}
#' There are three areas where the functions differ:
#' 1. input arguments,
#' 2. attributes of the returned matrix, and
#' 3. behavior at the right boundary knot.
#'
#' #### Input Arguments
#'
args(bsplines)
args(splines::bs)

#'
#' | cpr::bsplines | splines::bs    | Notes                                    |
#' |:--------      |:-----------    |:-----                                    |
#' | x             | x              | numeric vector; the predictor variable   |
#' | iknots        | knots          | internal knots                           |
#' | bknots        | Boundary.knots | boundary knots                           |
#' | order         | degree         | polynomial order = polynomial degree + 1 |
#' | df            | df             | degrees of freedom                       |
#' |               | intercept      |                                          |
#' |               | warn.outside   |                                          |
#'
{{ backtick(bsplines) }}
#' does not have the
{{ backtick(intercept) }}
#' nor the
{{ backtick(warn.outside) }}
#' arguments because the matrix generated by
{{ backtick(bsplines) }}
#' effectively _always_ has
{{ backtick("intercept = TRUE", dequote = TRUE) }}
#' and
{{ backtick("warn.outside = TRUE", dequote = TRUE) %s% "."}}
#' How values of
{{ backtick(x) }}
#' at the right boundary, and outside the boundary are treated also differ
#' between the two functions.
#'
#' #### Attributes of the returned matrices
#'
#' The default call for both B-spline functions returns a basis matrix for a
#' order 4 (degree 3; cubic) B-spline with boundary knots placed at
{{ backtick(range(x)) %s% "." }}
#' However, the returns are not the same.
bs_mat <- splines::bs(x, knots = attr(bmat, "iknots"), Boundary.knots = attr(bmat, "bknots"))
str(attributes(bmat))
str(attributes(bs_mat))

#'
#' The
{{ backtick(bspline_mat) }}
#' has additional attributes related to the control polygons.
#'
#' The major difference is the in the dimension of the matrices.
#' By default
{{ backtick(splines::bs) }}
#' omits one column from the basis matrix such that when using using the
#' function is a regression formula the resulting design matrix is not rank
#' deficient.  Using
{{ backtick(bsplines) }}
#' would suggest using a
{{ backtick(+0) %s% " or " %s% backtick(-1)}}
#' in the regression formula to omit the intercept (is nuance is handled in
#' calls to
{{ backtick(cp) }}
#' so the end user need not worry about it).
#'
#' #### Right Continuity
#'
#' By definition, the $\boldsymbol{B}_{j,k,\boldsymbol{\xi}}\left(x\right)$ are
#' non-negative right-continuous functions.
{{ backtick(bsplines) }}
#' adheres to the definition strictly, whereas
{{ backtick(splines::bs) }}
#' uses a pivoting method to allow for non-zero extrapolations outside the
#' support.
#'
#' Example: for the
{{ backtick(cpr::bsplines) }}
#' call, notice that the first, third, and fifth rows, corresponding to values
#' outside the support are all zeros as are the row sums.  Compare that to the
{{ backtick(splines::bs) }}
#' which returns negative values and in the matrix, and all rows sum to 1.
bspline_eg <- bsplines(c(0, 1, 2, 5, 6), bknots = c(1, 5))
bs_eg      <- splines::bs(c(0, 1, 2, 5, 6), Boundary.knots = c(1, 5), intercept = TRUE )

head(bspline_eg)
rowSums(bspline_eg)

head(bs_eg)
rowSums(bs_eg)

#'
#' # Control Polygons
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
#+ label = "define_theta"
theta <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
cp0 <- cp(bmat, theta)

#'
#' Plotting the control polygon and the corresponding spline:
#'
#+ label = "plot_cp", fig.width = 7, fig.height = 4
plot(cp0, show_spline = TRUE)

#'
#' # Knot Influence
#'
#' ## Spline Spaces and Inserting a Knot
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
#' For an example, insert a knot $\xi' = 3$ into the control polygon defined
#' above.
#+ label = "insert_xi_prime3", fig.width = 7, fig.height = 4
cp1 <- insert_a_knot(cp0, xi_prime = 3)
plot(cp0, cp1, color = TRUE, show_spline = TRUE)

#'
#' ## Assessing influence of $\xi_j$
#'
#' Here we derive a metric for assessing how much influence $\xi_j \in
#' \boldsymbol{\xi}$ has on $\boldsymbol{B}_{k,\boldsymbol{\xi}}
#' \left(x\right)\boldsymbol{\theta}_{\boldsymbol{\xi}}.$ Using the relationship
#' defined by @boehm1980, we can derive a metric for the influence of $\xi_j$
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
#'                &= \boldsymbol{\theta}_{\boldsymbol{\xi}} - \boldsymbol{W} \left(\boldsymbol{W}^{T}\boldsymbol{W}\right)^{-1}\boldsymbol{W}^{T}        \boldsymbol{\theta}_{\boldsymbol{\xi}} \\
#'                &= \left(\boldsymbol{I}                   - \boldsymbol{W} \left(\boldsymbol{W}^{T}\boldsymbol{W}\right)^{-1}\boldsymbol{W}^{T}\right) \boldsymbol{\theta}_{\boldsymbol{\xi}} \\
#' \end{align}
#' $$
#'
#' Finally, we define the influence of $\xi_j$ on
#' $CP_{k,\boldsymbol{\xi},\boldsymbol{\theta}_{\boldsymbol{\xi}}}$ as
#' $$ w_j = \left\lVert \boldsymbol{d} \right\rVert_{2}^{2}. $$
#'
#' The influence of knots on the spline used in the above section.
#'
#+ fig.width = 7, fig.height = 7
x <- influence_of_iknots(cp0)
summary(x)

#'
#' Let's look at the following plots to explore the influence of $\xi_{7}$ (the
#' third interior knot in a $k=4$ order spline) on the spline.  In panel (a)
#' we see the original control polygon and spline along with the coarsened
#' control polygon and spline. Note that the there are fewer control points, and
#' the spline deviates from the original. In panel (b) we see that the restored
#' control polygon is the result of inserting $\xi_7$ into the coarsened control
#' polygon of panel (a).  These plots are also a good example of the local
#' support and strong convexity of the control polygons as there are only $k + 1 = 5$ control points which are
#' impacted by the removal and re-insertion of $\xi_7.$  Lastly, in panel (c) we
#' see all three control polygons plotted together.
#+ fig.width = 7, fig.height = 7
ggpubr::ggarrange(
  ggpubr::ggarrange(
      plot(x, j = 3, coarsened = TRUE, restored = FALSE, color = TRUE, show_spline = TRUE) +
        ggplot2::theme(legend.position = "none")
    , plot(x, j = 3, coarsened = FALSE, restored = TRUE, color = TRUE, show_spline = TRUE) +
      ggplot2::theme(legend.position = "none")
    , labels = c("(a)", "(b)")
    , nrow = 1
  )
  , plot(x, j = 3, coarsened = TRUE, restored = TRUE, color = TRUE, show_spline = TRUE) +
    ggplot2::theme(legend.position = "bottom")
  , labels = c("", "(c)")
  , nrow = 2
  , ncol = 1
  , heights = c(1, 2)
)

#'
#' Next, consider the influence of $\xi_8,$ the fourth interior knot.  By the
#' influence metric defined above, this is the least influential knot in the
#' sequence.  This can be seen easily as the spline between the original and the
#' coarsened spline are very similar, this is despite the apparent large
#' difference in the magnitude of the control point ordinates between the
#' original and coarsened control polygons.  However, when re-inserting $\xi_8$
#' the recovered control polygon is very similar to the original, hence the low
#' influence of $\xi_8.$
#+ fig.width = 7, fig.height = 7
ggpubr::ggarrange(
  ggpubr::ggarrange(
      plot(x, j = 4, coarsened = TRUE, restored = FALSE, color = TRUE, show_spline = TRUE) +
        ggplot2::theme(legend.position = "none")
    , plot(x, j = 4, coarsened = FALSE, restored = TRUE, color = TRUE, show_spline = TRUE) +
      ggplot2::theme(legend.position = "none")
    , labels = c("(a)", "(b)")
    , nrow = 1
  )
  , plot(x, j = 4, coarsened = TRUE, restored = TRUE, color = TRUE, show_spline = TRUE) +
    ggplot2::theme(legend.position = "bottom")
  , labels = c("", "(c)")
  , nrow = 2
  , ncol = 1
  , heights = c(1, 2)
)

#'
#' If you were required to omit an internal knot, it would be preferable to omit
#' $\xi_8$ over $\xi_5, \xi_6, \xi_7,$ or $\xi_9$ as that will have the least
#' impact on the spline approximation of the original functional form.
#'
#' # Fitting B-splines to noisy data
#'
#' Start with the spline we have been using and add some noise to it.
#+ fig.width = 7, fig.height = 4
set.seed(42)
x <- seq(0, 5.99999, length.out = 100)
bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
theta <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
DF <- data.frame(x = x, truth = as.numeric(bmat %*% theta))
DF$y <- as.numeric(bmat %*% theta + rnorm(nrow(bmat), sd = 0.3))

original_data_ggplot_layers <-
  list(
    ggplot2::geom_point(data = DF
                        , mapping = ggplot2::aes(x = x, y = y)
                        , inherit.aes = FALSE
                        , color = "#6F263D"
                        , alpha = 0.2)
    ,
    ggplot2::geom_line(data = DF
                       , mapping = ggplot2::aes(x = x, y = truth)
                       , inherit.aes = FALSE
                       , color = "#6F263D"
                       , alpha = 0.6)
  )

ggplot2::ggplot(DF) + ggplot2::theme_bw() + original_data_ggplot_layers

#'
#' To fit a spline and control polygon to the noisy data use a formula statement
#' in the
{{qwraps2::backtick(cp)}}
#' call.  In this example we will use the known internal knots and add one
#' extra.
#+ fig.width = 7, fig.height = 4
initial_cp <-
  cp(y ~ bsplines(x, iknots = c(1, 1.5, 2.3, 3.0, 4, 4.5), bknots = c(0, 6))
     , data = DF
     , keep_fit = TRUE # default is FALSE
  )

plot(initial_cp, show_spline = TRUE) + original_data_ggplot_layers

#'
#' The plot above shows the fitted spline is does well at approximating the true
#' spline function.  Just to make it perfectly clear, the regression
#' coefficients are the estimates of the ordinates for the control polygon:
initial_cp$fit |> coef() |> unname()
initial_cp$cp$theta

#'
#' Let's now look at the influence of the internal knots on the fit
#+
summary(influence_of_iknots(initial_cp))

#'
#' The least influential knot is $\xi_8 = 3.0,$ the extra knot inserted.  Good,
#' we this is the expected result.
#'
#' How would someone determine if the influence was significant?  That is, how
#' can we test the null hypothesis $$H_0: w_{j} = 0$$
#'
#' Under the null hypothesis that the knot has zero influence and under standard
#' ordinary least squares regression assumptions the regression coefficients,
#' (the ordinates of the control polygon are the regression coefficients), are
#' realizations of a Gaussian random variable. Then
#'
#' $$
#' w_j =
#' \left(\left(\boldsymbol{I} - \boldsymbol{H}\right)\hat{\boldsymbol{\theta}} \right)^{T}
#' \left[
#'   \left(\left(\boldsymbol{I} - \boldsymbol{H}\right)\hat{\boldsymbol{\theta}} \right)
#'   \boldsymbol{\Sigma}
#'   \left(\left(\boldsymbol{I} - \boldsymbol{H}\right)\hat{\boldsymbol{\theta}} \right)^{T}
#' \right]^{+}
#' \left(\left(\boldsymbol{I} - \boldsymbol{H}\right)\hat{\boldsymbol{\theta}} \right)
#' $$
#' where $\boldsymbol{H} = W (W^{T}W)^{-1} W^{T},$ $\boldsymbol{\Sigma}$ is
#' the variance-covariance matrix for the regression coefficients, and $+$
#' denotes the Moore-Penrose inverse of the matrix.  By construction,
#' $\left(\left(\boldsymbol{I} - \boldsymbol{H}\right)\hat{\boldsymbol{\theta}} \right) \boldsymbol{\Sigma} \left(\left(\boldsymbol{I} - \boldsymbol{H}\right)\hat{\boldsymbol{\theta}} \right)^{T}$
#' is singular and thus the standard inverse does not exist and a generalized
#' inverse is necessary. This yields the test statistic following a chi-square
#' distribution with one degree of freedom,
#' $$ w_j \sim \chi_{1}^{2}.$$
#'
#' Now, if we are interested in removing the knot with the lowest influence we
#' are interested in the minimum.  So the hypothesis test we are actually
#' interested in is
#' $$ H_0: w_{(1)} = 0 $$ which follows the distribution
#' $$ \Pr \left[ w_{(1)} > w \right] = \sum_{j = 1}^{k+l} \binom{n+l}{j}
#' \left(F_{\chi_{1}^{2}}\left(w\right)\right)^{j} \left( 1 -
#' F_{\chi_{1}^{2}}\left(w\right) \right)^{k+l-j}$$
#' where $F_{\chi_{1}^{2}}\left(w\right)$ is the distribution function of the
#' chi-square distribution with one degree of freedom.
#'
#' The results generated by calling
{{ qwraps2::backtick(influence_of_iknots) %s% "." }}
#' report two sets of p-values.  The first is the p-value is the probability of
#' observed chisq value greater than reported, and the second p-value is the
#' probability of the rank order statistic exceeding the observed value.
#+ results = "asis"
initial_cp |>
  influence_of_iknots() |>
  summary() |>
  knitr::kable(row.names = TRUE)

#'
#' It is worth remembering how fraught binary classification of statistical
#' (non)significance can be.  Just because the p-value is low does not mean that
#' the knot is influential, just as a high p-value dose not mean that the knot
#' is not influential.  Sample size, over-fitting, and other factors can/will
#' lead to poor selection of a model if you only consider these p-values.
#'
#' That said, consider $\xi_9 = 4.0$ which has the lowest influence weight.  Let's
#' omit that knot and refit the model.
#+ results = "asis"
first_reduction_cp <-
  cp(y ~ bsplines(x, iknots = c(1, 1.5, 2.3, 3, 4.5), bknots = c(0, 6)), data = DF)
first_reduction_cp |>
  influence_of_iknots() |>
  summary() |>
  knitr::kable(row.names = TRUE)

#'
#' After omitting one knot and refitting the model we see that $\xi = 2.3$ is
#' the least influential.  Just for fun, let's omit that knot, and refit.  Let's
#' continue that process all the way down to zero knots.
#+ results = "asis"
second_reduction_cp <-
  cp(y ~ bsplines(x, iknots = c(1, 1.5, 3, 4.5), bknots = c(0, 6)), data = DF)
second_reduction_cp |>
  influence_of_iknots() |>
  summary() |>
  knitr::kable(row.names = TRUE)

#'
#' The least influential knot in the second reduction is $\xi = 1.5$ and that
#' will be omitted for the third reduction.
#+ results = "asis"
third_reduction_cp <-
  cp(y ~ bsplines(x, iknots = c(1, 3, 4.5), bknots = c(0, 6)), data = DF)
third_reduction_cp |>
  influence_of_iknots() |>
  summary() |>
  knitr::kable(row.names = TRUE)

#'
#' Within the third reduction, the least influential knot is $\xi = 3.0$ and
#' that knot will be omitted for the fourth reduction.
#+ results = "asis"
fourth_reduction_cp <-
  cp(y ~ bsplines(x, iknots = c(1, 4.5), bknots = c(0, 6)), data = DF)
fourth_reduction_cp |>
  influence_of_iknots() |>
  summary() |>
  knitr::kable(row.names = TRUE)

#'
#' Of the two remaining internal knots, $\xi - 1.0$ is the least influential and
#' will be omitted for the fifth reduction.
#+ results = "asis"
fifth_reduction_cp <-
  cp(y ~ bsplines(x, iknots = 4.5, bknots = c(0, 6)), data = DF)
fifth_reduction_cp |>
  influence_of_iknots() |>
  summary() |>
  knitr::kable(row.names = TRUE)

#'
#' Only one knot can be omitted from the fifth to the sixth reduction.  Having
#' the sixth reduction, where there are zero internal knots, lets us compare
#' model fits to a model with just a $k=4$ order polynomial.
sixth_reduction_cp <-
  cp(y ~ bsplines(x, bknots = c(0, 6)), data = DF)
sixth_reduction_cp |>
  influence_of_iknots() |>
  summary()

#'
#' Let's compare all the fits.  We will start by looking at the control polygons
#' and splines.
#'
#+ fig.height = 4, fig.width = 7, warning = FALSE, echo = FALSE
ggpubr::ggarrange(
  plot(  initial_cp , first_reduction_cp , second_reduction_cp , third_reduction_cp
       , fourth_reduction_cp , fifth_reduction_cp , sixth_reduction_cp
       , show_spline = FALSE , show_cp = TRUE , color = TRUE
       )
  ,
  plot(  initial_cp , first_reduction_cp , second_reduction_cp , third_reduction_cp
       , fourth_reduction_cp , fifth_reduction_cp , sixth_reduction_cp
       , show_spline = TRUE , show_cp = FALSE , color = TRUE
       )
  , labels = c("(a)", "(b)")
  , common.legend = TRUE
)

#'
#' The control polygons, panel (a), we see that the sixth, fifth, and fourth reductions are
#' different from the initial, first, second, and third reductions, and for the
#' splines, the fifth and sixth reductions are easily identified as different.
#'
#' Next, the graphic below will let us compare these models to the truth, and
#' the observed data.
#+ fig.width = 7, fig.height = 4, echo = FALSE
ggpubr::ggarrange(
    plot(initial_cp, show_spline = TRUE) +
      ggplot2::ggtitle("Initial CP") +
      ggplot2::coord_cartesian(ylim = c(-1, 5)) +
      original_data_ggplot_layers
  , plot(first_reduction_cp, show_spline = TRUE)  +
    ggplot2::ggtitle("First Reduction") +
    ggplot2::coord_cartesian(ylim = c(-1, 5)) +
    original_data_ggplot_layers
  , plot(second_reduction_cp, show_spline = TRUE) +
    ggplot2::ggtitle("Second Reduction") +
    ggplot2::coord_cartesian(ylim = c(-1, 5)) +
    original_data_ggplot_layers
  , plot(third_reduction_cp, show_spline = TRUE)  +
    ggplot2::ggtitle("Third Reduction") +
    ggplot2::coord_cartesian(ylim = c(-1, 5)) +
    original_data_ggplot_layers
  , plot(fourth_reduction_cp, show_spline = TRUE) +
    ggplot2::ggtitle("Fourth Reduction") +
    ggplot2::coord_cartesian(ylim = c(-1, 5)) +
    original_data_ggplot_layers
  , plot(fifth_reduction_cp, show_spline = TRUE) +
    ggplot2::ggtitle("Fifth Reduction") +
    ggplot2::coord_cartesian(ylim = c(-1, 5)) +
    original_data_ggplot_layers
  , common.legend = TRUE
  )

#'
#' The dashed black line is the spline fitted to the data (light burgundy dots) and
#' the true value of the target function is the burgundy line.  In the fifth
#' reduction there is an easily noticeable difference between the fitted spline
#' and the target function. Between the initial control polygon and the first
#' three reductions it is difficult to visually discern any meaningful difference
#' between the fits.
#'
#' Thus, I would argue that the third reduction is the preferable model as it
#' has the fewest degrees of freedom while providing a good quality of fit.
#' This conclusion is supported by looking at the residual standard error (rse)
#' $$ rse = \sqrt{ \frac{1}{df} \sum_{i=1}^{n} \left(y_i - f\left(x_i\right)\right)^2 },$$
#' where the degrees of freedom, $df,$ is the sample size $n$ minus the number
#' of regression parameters.
#+ results = 'asis'
list(  initial_cp , first_reduction_cp , second_reduction_cp , third_reduction_cp
     , fourth_reduction_cp , fifth_reduction_cp , sixth_reduction_cp) |>
  rev() |>
  lapply(summary) |>
  do.call(what = rbind, args = _) |>
  cbind(data.frame(reduction = seq(6, 0, by = -1))) |>
  knitr::kable(row.names = TRUE)

#'
#' The
{{ backtick(wiggle) }}
#' is one measure of wiggliness defined as
#' $$
#' \int_{\min\left(\boldsymbol{\xi}\right)}^{\max\left(\boldsymbol{\xi}\right)}
#' \left( \frac{d^2}{dx^2} f\left(x\right) \right)^2 dx.
#' $$
{{ backtick(fdsc) }}
#' reports the number of times the first derivative has a sign change.
#'
#' # Control Polygon Reduction
#'
#'
#' The exercise above of manually identifying and omitting the knot with the
#' smallest influence in each model would be tedious when working with a large
#' set of initial knots.  Fortunately, the process has been automated.  Calling
{{ qwraps2::backtick(cpr) }}
#' on a
{{ qwraps2::backtick(cpr_cp) }}
#' object defined by a function will automatically omit the internal knot with
#' the lowest influence.
#'
#' ## Example with known knots
#'
#' Apply CPR to the
{{ qwraps2::backtick(initial_cp) }}
#' from the above example.
#'
cpr0 <- cpr(initial_cp)
cpr0

#'
#' There are
{{ length(cpr0) }}
#' elements of the
{{ backtick(cpr_cpr) }}
#' object,
{{ backtick(length(initial_cp$iknots) + 1) %s% "."}}
#' The indexing is set such at that the $i^{th}$ element has $i-1$ internal
#' knots.
#'
#' Before exploring the results, let's just verify that the results of the call
#' to
{{ backtick(cpr) }}
#' are the same as the manual results found about.
#' There are some differences in the metadata of the objects, but the important
#' parts, like the control polygons, are the same.
all.equal( cpr0[[7]][["cp"]],  initial_cp[["cp"]])

# some attributes are different with the last cp due to how the automation
# creates the call vs how the call was created manually.
call_idx <- which(names(cpr0[[6]]) == "call")
all.equal( cpr0[[6]][-call_idx], first_reduction_cp [-call_idx])
all.equal( cpr0[[5]][-call_idx], second_reduction_cp[-call_idx])
all.equal( cpr0[[4]][-call_idx], third_reduction_cp [-call_idx])
all.equal( cpr0[[3]][-call_idx], fourth_reduction_cp[-call_idx])
all.equal( cpr0[[2]][-call_idx], fifth_reduction_cp [-call_idx])
all.equal( cpr0[[1]][-call_idx], sixth_reduction_cp [-call_idx], check.attributes = FALSE)

#'
#' In the manual process we identified
{{ backtick(third_reduction_cp) }}
#' as the preferable model. For the
{{ backtick(cpr0) }}
#' object we can quickly see a similar result as we did for the manual process.
#+ result = "asis"
summary(cpr0) |> knitr::kable(row.names = TRUE)

#'
#' The additional columns in this summary,
{{ backtick("loglik_elbow", dequote = TRUE) %s% " and " %s% backtick(rse_elbow) %s% ","}}
#' indicate a location in the plot for either the loglik or rse by model
#' index (degrees of freedom) where the trade-off between additional degrees of
#' freedom and improvement in the fix statistic is negligible.  See plot below.
#' This is determined by finding the breakpoint such that a continuous, but not
#' differentiable at breakpoint, quadratic spline fits the plot with minimal
#' residual standard error.
#+ fig.height = 7, fig.width = 7
ggpubr::ggarrange(
    plot(cpr0, type = "cps", color = TRUE)
  , plot(cpr0, type = "cps", show_cp = FALSE, show_spline = TRUE, color = TRUE)
  , plot(cpr0, type = "loglik")
  , plot(cpr0, type = "rse")
  , ncol =2
  , nrow = 2
  , common.legend = TRUE
)

#'
#' ## Example when knots are unknown
#'
#' In practice it is be extremely unlikely to know where knots should be placed.
#' Analytic solutions are difficult, if not impossible to derive
#' [@jupp1978approximation].
#' However, an optimal solution may not be necessary.
#'
#' From @deboor2001 (page 106)
#' <quote>
#' "...a B-spline doesn't change much if one changes its $k+1$ knots a little
#' bit. Therefore, if one has multiple knots, then it is very easy to find
#' B-spline almost like with simple knots: Simply replace knot of multiplicity
#' $r > 1$ by $r$ simple knots nearby."
#' </quote>
#'
#' That is,
#'
#' $$
#'   \boldsymbol{B}_{k, \boldsymbol{\xi}}\left(x\right) \boldsymbol{\theta}_{\boldsymbol{\xi}} \approx
#'   \boldsymbol{B}_{k, \boldsymbol{\xi'}}\left(x\right) \boldsymbol{\theta}_{\boldsymbol{\xi'}}
#' $$
#' where
#' $$
#' \left\lvert \xi_j - \xi_j' \right\rvert < \delta \quad \text{for small} \quad \delta > 0.
#' $$
#'
#' So, in the case when we are looking for a good set of knots (a good set of
#' knots should be parsimonious, and provide a good model fit) we start with an
#' initial knot sequence with a high cardinality, this is not without precedent
#' [@eilers1996,@eilers2010].
#' We then apply the CPR algorithm to find a good set of knots.
#'
#' For example we will use 50 internal knots. Not surprisingly we have a fit
#' that is more "connect-the-dots" than a smooth fit.
#+ fig.width = 7, fig.height = 4
initial_cp <- cp(y ~ bsplines(x, df = 54, bknots = c(0, 6)), data = DF)

ggpubr::ggarrange(
  plot(initial_cp, show_cp = TRUE, show_spline = FALSE) + original_data_ggplot_layers
  ,
  plot(initial_cp, show_cp = FALSE, show_spline = TRUE) + original_data_ggplot_layers
)

#'
#' Apply CPR to the
{{ backtick(initial_cp) }}
#' and look at the summary.  Only the first 10 of 51 rows are provided here.
cpr1 <- cpr(initial_cp)
x <- summary(cpr1)
knitr::kable(head(x, 10))

#'
#' From this, the preferable model is suggested to be index 5, the model with
#' four internal knots.  Inspection of the rse by index plot, I would argue from
#' a manual selection that index 5 is preferable overall.
plot(cpr1, type = "rse")

#'
#' Let's compare the models in indices 3, 4, and 5.
#+ fig.width = 7, fig.height = 4
ggpubr::ggarrange(
  plot(cpr1[[3]], cpr1[[4]], cpr1[[5]], show_cp = TRUE, show_spline = FALSE, color = TRUE)
  ,
  plot(cpr1[[3]], cpr1[[4]], cpr1[[5]], show_cp = FALSE, show_spline = TRUE, color = TRUE)
  ,
  common.legend = TRUE
)

#'
#' The noticeable differences are, for the most part, located on the left side
#' of the support between between 0 and 1.  For the fifth index, there is change
#' in convexity of the spline. Knowing that the models at index 3 and 4 are good
#' fits too, then it would be easy not select index 5 due to extra "wiggle" in
#' the spline.
#'
#' In practice I would likely pick the model in index 4 to have a smooth (small
#' wiggle) and low rse.  Compare the selected model to the original data.
#'
#+ fig.width = 7, fig.height = 4
ggpubr::ggarrange(
    plot(cpr1[[3]], show_cp = FALSE, show_spline = TRUE) +
    ggplot2::ggtitle("Model Index 3") +
    original_data_ggplot_layers
  , plot(cpr1[[4]], show_cp = FALSE, show_spline = TRUE) +
    ggplot2::ggtitle("Model Index 4") +
    original_data_ggplot_layers
  , plot(cpr1[[5]], show_cp = FALSE, show_spline = TRUE) +
    ggplot2::ggtitle("Model Index 5") +
    original_data_ggplot_layers
  , nrow = 1
  , legend = "none"
)

#'
#' # Extensions to higher dimensions
#'
#' CPR works for uni-variable B-splines.  By taking tensor products of
#' B-splines, and building a control-net, the higher-dimensional analog of a
#' control polygon, we can apply similar methods to estimate a surface.
#' Details on the Control Net Reduction method are presented in
#+ echo = TRUE, eval = FALSE
# /*  don't evaluate the vignette call, but show it in the vignette
while(FALSE) {
# */
vignette(topic = "cnr", package = "cpr")
# /*
}
# */
#'
#' # References
#'<div id="refs"></div>
#'
#' # Session Info
#+ label = "sessioninfo"
sessionInfo()

# /* ---------------------------- END OF FILE ------------------------------- */
