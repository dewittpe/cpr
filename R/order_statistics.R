#' Distribution of Order Statistics
#'
#' Density of distribution function for the jth order statisitic from a sample
#' of size n from a known distribution function.
#'
#' For a known distribution with defined density and distribution functions,
#' e.g., normal (\code{\link[stats]{dnorm}}, \code{\link[stats]{pnorm}}), or
#' chisq (\code{\link[stats]{dchisq}}, \code{\link[stats]{pchisq}}), we define
#' the density function of of the jth order statistic, from a sample of size n,
#' to be
#' \deqn{ \frac{n!}{(j-1)!(n-j)!} f(x) F(x)^{j-1} (1 - F(x))^{n-j}}{n!/((j-1)!(n-j)!) f(x) (F(x))^(j-1) (1-F(x))^(n-j)}.
#'
#' and the distribution function to be
#'
#' \deqn{\sum_{k = j}^{n} \binom{n}{k} \left[F(x)\right]^{k} \left[1-F(x)\right]^{n-k}}{sum(dbinom(x = j:n, size = n, prob = F(x)))}.
#'
#' @param x,q vector or quantiles
#' @param n sample size
#' @param j jth order statisitic
#' @param distribution character string defining the distributioSe see Details
#' @param ... additional arguments passed to the density and distribution
#' function
#'
#' @references
#' Casella G, Berger RL (2002). Statistical Inference. 2nd edition. Duxbury Thomson Learning.
#'
#' @examples
#'
#' # Example 1
#' # Find the distribution of the minimum from a sample of size 54 from a
#' # standard normal distribution
#'
#' simulated_data <- matrix(rnorm(n = 54 * 5000), ncol = 54)
#'
#' # find all the minimums for each of the simulated samples of size 54
#' mins <- apply(simulated_data, 1, min)
#'
#' # get the density values
#' x <- seq(-5, 0, length.out = 100)
#' d <- d_order_statistic(x, n = 54, j = 1, distribution = "norm")
#'
#' # plot the histogram and density
#' hist(mins, freq = FALSE)
#' points(x, d, type = "l", col = "red")
#'
#' # plot the distribution function
#' plot(ecdf(mins))
#' points(x, p_order_statistic(q = x,  n = 54, j = 1, distribution = "norm"), col = "red")
#'
#' # Example 2
#' # Find the density and distrubition of the fourth order statistic from a
#' # sample of size 12 from a chisq distribution with 3 degrees of freedom
#'
#' simulated_data <- matrix(rchisq(n = 12 * 5000, df = 3), ncol = 12)
#'
#' os4 <- apply(simulated_data, 1, function(x) sort(x)[4])
#'
#' x <- seq(min(os4), max(os4), length.out = 100)
#' d <- d_order_statistic(x, n = 12, j = 4, distribution = "chisq", df = 3)
#' p <- p_order_statistic(x, n = 12, j = 4, distribution = "chisq", df = 3)
#'
#' hist(os4, freq = FALSE); points(x, d, type = "l", col = "red")
#' plot(ecdf(os4)); points(x, p, col = "red")
#'
#' @name order_statistics
NULL

#' @export
#' @rdname order_statistics
d_order_statistic <- function(x, n, j, distribution, ...) {
  n <- as.integer(n)
  j <- as.integer(j)

  stopifnot(length(n) == 1, length(q) == length(j), !is.na(n), n >= stats::na.omit(j), stats::na.omit(j) >= 1)

  dfun <- match.fun(FUN = paste0("d", distribution))
  pfun <- match.fun(FUN = paste0("p", distribution))

  d <- do.call(dfun, c(list(x = x), ...))
  p <- do.call(pfun, c(list(q = x), ...))

  exp( lgamma(n+1) - lgamma(j) - lgamma(n - j + 1) + log(d) + (j - 1) * log(p) + (n - j) * log(1 - p) )

}

#' @export
#' @rdname order_statistics
p_order_statistic <- function(q, n, j, distribution, ...) {
  n <- as.integer(n)
  j <- as.integer(j)
  stopifnot(length(n) == 1, length(q) == length(j), !is.na(n), n >= stats::na.omit(j), stats::na.omit(j) >= 1)
  if (length(q) == 0) {
    return( numeric(0) )
  }

  pfun <- match.fun(FUN = paste0("p", distribution))
  p <- do.call(pfun, list(q = q, ...))

  rtn <-
    mapply(stats::dbinom,
           x =
             lapply(j, function(from) if(is.na(from)) {from} else { seq(from = from, to = n, by = 1)}) ,
           prob = p,
           MoreArgs = list(size = n))

  sapply(rtn, sum)
}

