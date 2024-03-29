% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_surface.R
\name{get_surface}
\alias{get_surface}
\title{Get Surface}
\usage{
get_surface(x, margin = 1:2, at, n = 100)
}
\arguments{
\item{x}{a \code{cpr_cn} object}

\item{margin}{an integer identifying the marginal of the control net to slice
along.  Only used when working \code{x} is a \code{cpr_cn} object.}

\item{at}{point value for marginals not defined in the \code{margin}.  Only
used when \code{x} is a \code{cpr_cn} object.  Expected input is a list of
length \code{length(attr(x, "bspline_list"))}.  Entries for elements
\code{marginal} are ignored.  If omitted, the midpoint between the boundary
knots for each marginal is used.}

\item{n}{the length of sequence to use for interpolating the spline function.}
}
\value{
a list with two elements
\describe{
\item{cn}{the control net}
\item{surface}{a data.frame with three columns to define the surface}
}
}
\description{
Get Two-Dimensional Control Net and Surface from n-dimensional Control Nets
}
\examples{
\donttest{
## Extract the control net and surface from a cpr_cn object.
a_cn <- cn(log10(pdg) ~ btensor(list(day, age, ttm)
           , df = list(15, 3, 5)
           , bknots = list(c(-1, 1), c(45, 53), c(-9, -1))
           , order = list(3, 2, 3))
           , data = spdg)

cn_and_surface <- get_surface(a_cn, n = 50)
str(cn_and_surface, max.level = 2)

old_par <- par()
par(mfrow = c(1, 2))
with(cn_and_surface$cn,
     plot3D::persp3D(unique(Var1),
                     unique(Var2),
                     matrix(z,
                            nrow = length(unique(Var1)),
                            ncol = length(unique(Var2))),
                     main = "Control Net")
     )
with(cn_and_surface$surface,
     plot3D::persp3D(unique(Var1),
                     unique(Var2),
                     matrix(z,
                            nrow = length(unique(Var1)),
                            ncol = length(unique(Var2))),
                     main = "Surface")
     )

par(old_par)
}
}
\seealso{
\code{\link{get_spline}}
}
