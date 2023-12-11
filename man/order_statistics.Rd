% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/order_statistics.R
\name{order_statistics}
\alias{order_statistics}
\alias{d_order_statistic}
\alias{p_order_statistic}
\title{Distribution of Order Statistics}
\usage{
d_order_statistic(x, n, j, distribution, ...)

p_order_statistic(q, n, j, distribution, ...)
}
\arguments{
\item{x, q}{vector or quantiles}

\item{n}{sample size}

\item{j}{jth order statisitic}

\item{distribution}{character string defining the distributioSe see Details}

\item{...}{additional arguments passed to the density and distribution
function}
}
\description{
Density of distribution function for the jth order statisitic from a sample
of size n from a known distribution function.
}
\details{
For a known distribution with defined density and distribution functions,
e.g., normal (\code{\link[stats]{dnorm}}, \code{\link[stats]{pnorm}}), or
chisq (\code{\link[stats]{dchisq}}, \code{\link[stats]{pchisq}}), we define
the density function of of the jth order statistic, from a sample of size n,
to be
\deqn{ \frac{n!}{(j-1)!(n-j)!} f(x) F(x)^{j-1} (1 - F(x))^{n-j}}{n!/((j-1)!(n-j)!) f(x) (F(x))^(j-1) (1-F(x))^(n-j)}.

and the distribution function to be

\deqn{\sum_{k = j}^{n} \binom{n}{k} \left[F(x)\right]^{k} \left[1-F(x)\right]^{n-k}}{sum(dbinom(x = j:n, size = n, prob = F(x)))}.
}
\examples{

# Example 1
# Find the distribution of the minimum from a sample of size 54 from a
# standard normal distribution

simulated_data <- matrix(rnorm(n = 54 * 5000), ncol = 54)

# find all the minimums for each of the simulated samples of size 54
mins <- apply(simulated_data, 1, min)

# get the density values
x <- seq(-5, 0, length.out = 100)
d <- d_order_statistic(x, n = 54, j = 1, distribution = "norm")

# plot the histogram and density
hist(mins, freq = FALSE)
points(x, d, type = "l", col = "red")

# plot the distribution function
plot(ecdf(mins))
points(x, p_order_statistic(q = x,  n = 54, j = 1, distribution = "norm"), col = "red")

# Example 2
# Find the density and distrubition of the fourth order statistic from a
# sample of size 12 from a chisq distribution with 3 degrees of freedom

simulated_data <- matrix(rchisq(n = 12 * 5000, df = 3), ncol = 12)

os4 <- apply(simulated_data, 1, function(x) sort(x)[4])

x <- seq(min(os4), max(os4), length.out = 100)
d <- d_order_statistic(x, n = 12, j = 4, distribution = "chisq", df = 3)
p <- p_order_statistic(x, n = 12, j = 4, distribution = "chisq", df = 3)

hist(os4, freq = FALSE); points(x, d, type = "l", col = "red")
plot(ecdf(os4)); points(x, p, col = "red")

}
\references{
Casella G, Berger RL (2002). Statistical Inference. 2nd edition. Duxbury Thomson Learning.
}