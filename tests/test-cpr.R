library(cpr)

initial_cp <- cp(log10(pdg) ~ bsplines(day, df = 54), data = spdg)
cpr_run <- cpr(initial_cp)
s <- summary(cpr_run)

################################################################################
# test that cpr is as expected

stopifnot(identical(s$index, as.integer(1:51)))
stopifnot(identical(s$dfs,      as.integer(4:54)))
stopifnot(identical(s$n_iknots, as.integer(0:50)))

stopifnot(all.equal(s$rmse,     c(0.356834016392611, 0.349277946045147, 0.34779881030238, 0.346775617941792, 0.34672681446779, 0.346724763978048, 0.346720165278699, 0.346720127475827, 0.346687093279954, 0.346673369130874, 0.346662945142717, 0.346662698692668, 0.346640800103193, 0.346632411566484, 0.346594289182594, 0.346593550321772, 0.34656533261078, 0.346565278806961, 0.346522063937811, 0.346512387189849, 0.346467711881127, 0.346466903121175, 0.346466891093361, 0.346422731796281, 0.346417172445661, 0.346395072583581, 0.346390077884616, 0.346388524060614, 0.346386002151248, 0.346383374438381, 0.346382053831038, 0.346380619559524, 0.346380264892643, 0.346379742038133, 0.346379732642439, 0.346379732337148, 0.346372337432528, 0.346371045441169, 0.346370218754323, 0.346369473938062, 0.346369393057673, 0.346368754005226, 0.34636670064495, 0.346366087673203, 0.346365964395427, 0.346365828120997, 0.346365790280359, 0.346365745845726, 0.346365736240023, 0.346365733209633, 0.346365733125537)))
stopifnot(all.equal(s$loglik,   c(-9566.84481171829, -9039.73859055744, -8935.22152148567, -8862.66140795421, -8859.19514192969, -8859.04949524596, -8858.72284560794, -8858.72016041886, -8856.37358498029, -8855.39862790702, -8854.65808691684, -8854.64057834246, -8853.08478506372, -8852.48879237686, -8849.78007365508, -8849.72757225992, -8847.72241643258, -8847.71859296718, -8844.64741929779, -8843.95966431407, -8840.78420947433, -8840.72672024035, -8840.72586526403, -8837.5866779129, -8837.19144753933, -8835.62024146619, -8835.2651257538, -8835.15465012013, -8834.97534334278, -8834.7885125888, -8834.69461668603, -8834.59263879312, -8834.56742161894, -8834.53024608707, -8834.52957804229, -8834.52955633574, -8834.00376444302, -8833.91190022898, -8833.85312029284, -8833.80016148289, -8833.79441062256, -8833.7489718555, -8833.60297050232, -8833.5593858231, -8833.55062028362, -8833.54093062686, -8833.53824000551, -8833.53508052429, -8833.53439752006, -8833.53418204712, -8833.53417606757)))

# Test that the iknots are as expected.  This would suck to write
# out explicitly, so, there is some commented out code at the
# bottome of this file to write the needed expressions.
stopifnot(identical(length(s$iknots), 51L))

expected_iknots <- numeric(0)
stopifnot(identical(s$iknots[[1]], expected_iknots))

# the following tests can be updated, if needed, easily by using the
# code at the bottom of this file.
expected_iknots <- sort(c(expected_iknots, -0.0593128517003949))
stopifnot(all.equal(current = s$iknots[[2L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, 0.0827048768225241))
stopifnot(all.equal(current = s$iknots[[3L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.137070502182064))
stopifnot(all.equal(current = s$iknots[[4L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.378312135665077))
stopifnot(all.equal(current = s$iknots[[5L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.862130432110148))
stopifnot(all.equal(current = s$iknots[[6L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.88708372237784))
stopifnot(all.equal(current = s$iknots[[7L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.939778406906435))
stopifnot(all.equal(current = s$iknots[[8L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.836248012718601))
stopifnot(all.equal(current = s$iknots[[9L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.809851868675398))
stopifnot(all.equal(current = s$iknots[[10L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.678991596638655))
stopifnot(all.equal(current = s$iknots[[11L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.699049316696375))
stopifnot(all.equal(current = s$iknots[[12L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.646646894051738))
stopifnot(all.equal(current = s$iknots[[13L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.592728758169935))
stopifnot(all.equal(current = s$iknots[[14L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.539154539154539))
stopifnot(all.equal(current = s$iknots[[15L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.518443392682744))
stopifnot(all.equal(current = s$iknots[[16L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.459785577432636))
stopifnot(all.equal(current = s$iknots[[17L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.352286542597961))
stopifnot(all.equal(current = s$iknots[[18L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.320280112044818))
stopifnot(all.equal(current = s$iknots[[19L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.299470058293588))
stopifnot(all.equal(current = s$iknots[[20L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.189492013021425))
stopifnot(all.equal(current = s$iknots[[21L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.959346405228758))
stopifnot(all.equal(current = s$iknots[[22L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.161822999470058))
stopifnot(all.equal(current = s$iknots[[23L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.111297852474323))
stopifnot(all.equal(current = s$iknots[[24L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.0398190045248869))
stopifnot(all.equal(current = s$iknots[[25L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.0132485426603076))
stopifnot(all.equal(current = s$iknots[[26L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.27128197716433))
stopifnot(all.equal(current = s$iknots[[27L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.914066496163683))
stopifnot(all.equal(current = s$iknots[[28L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.758067490319012))
stopifnot(all.equal(current = s$iknots[[29L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.433121356650768))
stopifnot(all.equal(current = s$iknots[[30L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.48081336238199))
stopifnot(all.equal(current = s$iknots[[31L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.216953526416442))
stopifnot(all.equal(current = s$iknots[[32L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, 0.910716158121002))
stopifnot(all.equal(current = s$iknots[[33L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, 0.77688651218063))
stopifnot(all.equal(current = s$iknots[[34L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, 0.847617780143732))
stopifnot(all.equal(current = s$iknots[[35L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, 0.709507332344702))
stopifnot(all.equal(current = s$iknots[[36L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, 0.636618283677107))
stopifnot(all.equal(current = s$iknots[[37L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.78242220801364))
stopifnot(all.equal(current = s$iknots[[38L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.62116477532299))
stopifnot(all.equal(current = s$iknots[[39L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.406681190994917))
stopifnot(all.equal(current = s$iknots[[40L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, 0.213106295149639))
stopifnot(all.equal(current = s$iknots[[41L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, 0.278400248988485))
stopifnot(all.equal(current = s$iknots[[42L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, 0.354918437963421))
stopifnot(all.equal(current = s$iknots[[43L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, 0.4159938485198))
stopifnot(all.equal(current = s$iknots[[44L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.084920634920635))
stopifnot(all.equal(current = s$iknots[[45L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, 0.576293675364883))
stopifnot(all.equal(current = s$iknots[[46L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.24139979920913))
stopifnot(all.equal(current = s$iknots[[47L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, 0.144092931290163))
stopifnot(all.equal(current = s$iknots[[48L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.727465433347786))
stopifnot(all.equal(current = s$iknots[[49L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, -0.566268826371128))
stopifnot(all.equal(current = s$iknots[[50L]], target = expected_iknots))

expected_iknots <- sort(c(expected_iknots, 0.480392156862745))
stopifnot(all.equal(current = s$iknots[[51L]], target = expected_iknots))

################################################################################
# test that there is an error in the plotting method if type is not loglik or
# rmse
e <- try(plot(cpr_run, type = "not-a-type"), silent = TRUE)
stopifnot(inherits(e, "try-error"))
stopifnot(attr(e, "condition")$message == "type needs to be either 'cps', 'loglik', or 'rmse'.")

################################################################################
################################################################################
### #
### # The following code is helpful for creating the tests for the iknots.
### #
### # define a function for finding unique values between numeric
### # vectors with a tollerance
###
###
### find_unique <- function(x, y, tol = sqrt(.Machine$double.eps)) {
###   lwr <- y - tol
###   upr <- y + tol
###   z <- sapply(x, function(xx) { any(lwr < xx & xx < upr )})
###   x[!z]
### }
###
### expected_iknots <- numeric(0)
### # create the expressions and print them to the console
### for(i in 2:(length(s$iknots))) {
###   d <- (find_unique(s$iknots[[i]], expected_iknots))
###   e1 <- substitute(expected_iknots <- sort(c(expected_iknots, dd)), list(dd = d))
###   e2 <- substitute(expect_equal(current = s$iknots[[ii]], target = expected_iknots), list(ii = i))
###   print(e1)
###   eval(e1)
###   print(e2)
### }
###
################################################################################
###                               End of File                                ###
################################################################################

