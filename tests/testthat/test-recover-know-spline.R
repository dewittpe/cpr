recover_spline <- function(k = 4L, start_with = 100L, seed, theta_dist_sd = 100, ...) { 

  if (missing(seed)) {
    seed <- round(stats::runif(1) * 1e9)
  }

  set.seed(seed)

  n_iknots    <- sample(seq(1, floor(start_with / 4), by = 1L), 1L)
  true_iknots <- sort(stats::runif(n_iknots))
  true_theta  <- stats::rnorm(n_iknots + k, sd = theta_dist_sd)

  xvec <- seq(0, 1, length = 10001)

  true_bmat <- cpr::bsplines(xvec, iknots = true_iknots, order = k)

  true_cp <- cpr::cp(true_bmat, true_theta, ...)

  s_data <- dplyr::data_frame(x = xvec, 
                              y = as.numeric(true_bmat %*% matrix(true_theta, ncol = 1)))

  initial_iknots <- sort(c(stats::runif(start_with - n_iknots), true_iknots))

  f <- paste("y ~ cpr::bsplines(x, iknots = initial_iknots, order =", k, ")")
  f <- stats::as.formula(f)
  environment(f) <- environment()

  initial_cp <- do.call(cpr::cp, list(formula = f, data = s_data))

  cpr_run <- cpr::cpr(initial_cp, progress = FALSE)

  found_cp <- cpr_run[[n_iknots + 1L]]

  out <- 
    list(recovered  = isTRUE(all.equal(as.matrix(true_cp$cp), as.matrix(found_cp$cp))),
         call       = match.call(),
         n_iknots   = n_iknots,
         start_with = start_with,
         true_cp    = true_cp,
         initial_cp = initial_cp,
         found_cp   = found_cp,
         cpr_run    = cpr_run,
         seed       = seed
         )

  class(out) <- c("cprtesting_recover_spline", class(out))
  out
}

test_that("A known spline can be recovered.",
          {
            set.seed(42)
            expect_true(recover_spline(start_with = 40L, progress = FALSE)$recovered) 
          })
