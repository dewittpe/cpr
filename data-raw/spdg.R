################################################################################
# Simulation of Hormone Data set
# 
# This simulation is based on a subset of the Daily Hormone Study, part of the
# Study of Women's Health Across the Nation.
#
# Goal: create the spdg data.frame
#
################################################################################
set.seed(42)

# number of subjects to simulate
SUBJECTS <- 864

################################################################################
# load summary statistics and densities
################################################################################

dhs_summary <- readRDS("dhs_summary.rds")
list2env(dhs_summary, .GlobalEnv)
agettm_sccm_ch <- sccm::convex_hull(agettm_convexhull)

################################################################################
# Simulate age and time to menopause (ttm)
delta_age <- diff(agettm_kernel$x[1:2])
delta_ttm <- diff(agettm_kernel$y[1:2])

agettm <- with(agettm_kernel,
               {
                 out <- expand.grid(age = x, ttm = y)
                 out$z <- c(z)
                 as.data.frame(out)
               })

# Need to remove centers out of the convex hull from agettm
agettm <- agettm[which(sccm::is_in(agettm$age, agettm$ttm, agettm_sccm_ch) == 1), ]

index <- sample(seq_along(agettm$z), SUBJECTS - nrow(agettm_convexhull), replace = TRUE, prob = agettm$z)
spdg <- data.frame(age = agettm$age[index], ttm = agettm$ttm[index])

for(i in 1:nrow(spdg)) {
  repeat {
    age <- spdg$age[i] + runif(1, -delta_age/2, delta_age/2)
    ttm <- spdg$ttm[i] + runif(1, -delta_ttm/2, delta_ttm/2)

    if (sccm::is_in(age, ttm, agettm_sccm_ch) == 1) {
      spdg$age[i] <- age
      spdg$ttm[i] <- ttm
      break 
    }
  }
}

spdg <- rbind(spdg, agettm_convexhull)
spdg <- cbind(data.frame(id = 1:SUBJECTS), spdg)

spdg$ethnicity <-
  sample(c("Caucasian", "Black", "Chinese", "Hispanic", "Japanese"),
         size    = SUBJECTS,
         replace = TRUE,
         prob    = c(32, 18, 20, 7, 25))

spdg$ethnicity <-
  factor(spdg$ethnicity, levels = c("Caucasian", "Black", "Chinese", "Hispanic", "Japanese"))

# Check, visual check, swan vs spdg
# par(mfrow = c(2, 2))
# image(agettm_kernel, main = "SWAN")
# image(MASS::kde2d(spdg$age, spdg$ttm), main = "Simulated") 
# plot(agettm_sccm_ch)
# plot(sccm::convex_hull(spdg))

################################################################################
# Add BMI to the data
delta_bmi <- diff(agebmi_kernel$y[1:2])

spdg$bmi <-
  sapply(spdg$age, 
         function(a) {
           index <- which( (a > agebmi_kernel$x - delta_age/2) & (a < agebmi_kernel$x + delta_age/2)) 
           sample(agebmi_kernel$y, 1, prob = agebmi_kernel$z[index, ]) + runif(1, -delta_bmi/2, delta_bmi/2)
         })

# Visual check of age and bmi density
# par(mfrow = c(1, 2))
# image(agebmi_kernel, main = "SWAN")
# image(MASS::kde2d(spdg$age, spdg$bmi), main = "Simulated")

################################################################################
# Cycle lengths
index <- sample(seq_along(flll_kernel$z), SUBJECTS, replace = TRUE, prob = flll_kernel$z)
flll  <- flll_kernel[index, c("fl", "ll")]
flll  <- 
  Map(function(id, dfd) { data.frame(id = id, day_from_dlt = dfd) },
      id = 1:SUBJECTS,
      dfd = apply(flll, 1, function(x) seq(x[1], x[2], by = 1)))
flll <- do.call(rbind, flll)

spdg <- merge(spdg, flll, all = TRUE, by = "id")

spdg <- split(spdg, f = spdg$id)
spdg <-
  lapply(spdg, function(x) { 
           x$day_of_cycle <- seq_along(x$day_from_dlt)
           x$day <- NA_real_
           idx <- which(x$day_from_dlt > 0)
           x$day[idx] <- x$day_from_dlt[idx] / max(x$day_from_dlt)
           idx <- which(x$day_from_dlt <= 0)
           x$day[idx] <- x$day_from_dlt[idx] / -min(x$day_from_dlt)
           x}
  )
spdg <- do.call(rbind, spdg)

################################################################################
# the model

X <- model.matrix( ~ 0 + 
                  cpr::btensor(list(day, age, ttm, bmi), 
                               iknots = list(c(-0.0384, 0.0705), numeric(0), numeric(0), numeric(0)),
                               order  = list(3, 2, 2, 2)) + 
                  I(ethnicity == "Black") + 
                  I(ethnicity == "Chinese") + 
                  I(ethnicity == "Hispanic") +
                  I(ethnicity == "Japanese"),
                  data = spdg)
X <- X[, colnames(X) != "I(ethnicity == \"Black\")FALSE"]

error <- apply(X, 1, function(x) sqrt(matrix(x, nrow = 1) %*% Sigma %*% matrix(x, ncol = 1)))
mu    <- as.numeric(X %*% Theta)
rint  <- rep(runif(SUBJECTS, -0.5, 0.5), times = table(spdg$id))

spdg$pdg <- 10^(apply(cbind(mu, error), 1, function(x) rnorm(1, x[1], 15 * x[2])) + rint)

################################################################################
# Write out the data
save(spdg, file = "../data/spdg.rda")


################################################################################
# end of file
################################################################################
