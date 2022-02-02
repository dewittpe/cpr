test_that("cpr is as expected",
          {

            initial_cp <- cp(log10(pdg) ~ bsplines(day, df = 54), data = spdg)
            cpr_run <- cpr(initial_cp)
            s <- summary(cpr_run)

            expect_equal(object = s$index,    expected = 1:51)
            expect_equal(object = s$dfs,      expected = 4:54)
            expect_equal(object = s$n_iknots, expected = 0:50)
            expect_equal(object = s$rmse,     expected = c(0.356834016392611, 0.349277946045147, 0.34779881030238, 0.346775617941792, 0.34672681446779, 0.346724763978048, 0.346720165278699, 0.346720127475827, 0.346687093279954, 0.346673369130874, 0.346662945142717, 0.346662698692668, 0.346640800103193, 0.346632411566484, 0.346594289182594, 0.346593550321772, 0.34656533261078, 0.346565278806961, 0.346522063937811, 0.346512387189849, 0.346467711881127, 0.346466903121175, 0.346466891093361, 0.346422731796281, 0.346417172445661, 0.346395072583581, 0.346390077884616, 0.346388524060614, 0.346386002151248, 0.346383374438381, 0.346382053831038, 0.346380619559524, 0.346380264892643, 0.346379742038133, 0.346379732642439, 0.346379732337148, 0.346372337432528, 0.346371045441169, 0.346370218754323, 0.346369473938062, 0.346369393057673, 0.346368754005226, 0.34636670064495, 0.346366087673203, 0.346365964395427, 0.346365828120997, 0.346365790280359, 0.346365745845726, 0.346365736240023, 0.346365733209633, 0.346365733125537))
            expect_equal(object = s$loglik,   expected = c(-9566.84481171829, -9039.73859055744, -8935.22152148567, -8862.66140795421, -8859.19514192969, -8859.04949524596, -8858.72284560794, -8858.72016041886, -8856.37358498029, -8855.39862790702, -8854.65808691684, -8854.64057834246, -8853.08478506372, -8852.48879237686, -8849.78007365508, -8849.72757225992, -8847.72241643258, -8847.71859296718, -8844.64741929779, -8843.95966431407, -8840.78420947433, -8840.72672024035, -8840.72586526403, -8837.5866779129, -8837.19144753933, -8835.62024146619, -8835.2651257538, -8835.15465012013, -8834.97534334278, -8834.7885125888, -8834.69461668603, -8834.59263879312, -8834.56742161894, -8834.53024608707, -8834.52957804229, -8834.52955633574, -8834.00376444302, -8833.91190022898, -8833.85312029284, -8833.80016148289, -8833.79441062256, -8833.7489718555, -8833.60297050232, -8833.5593858231, -8833.55062028362, -8833.54093062686, -8833.53824000551, -8833.53508052429, -8833.53439752006, -8833.53418204712, -8833.53417606757))

            # Test that the iknots are as expected.  This would suck to write
            # out explicitly, so, there is some commented out code at the
            # bottome of this file to write the needed expressions.
            expect_identical(length(s$iknots), 51L)

            expected_iknots <- numeric(0)
            expect_identical(object = s$iknots[[1]], expected = expected_iknots)

            # the following tests can be updated, if needed, easily by using the
            # code at the bottom of this file.
            expected_iknots <- sort(c(expected_iknots, -0.0593128517003949))
            expect_equal(object = s$iknots[[2L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, 0.0827048768225241))
            expect_equal(object = s$iknots[[3L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.137070502182064))
            expect_equal(object = s$iknots[[4L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.378312135665077))
            expect_equal(object = s$iknots[[5L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.862130432110148))
            expect_equal(object = s$iknots[[6L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.88708372237784))
            expect_equal(object = s$iknots[[7L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.939778406906435))
            expect_equal(object = s$iknots[[8L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.836248012718601))
            expect_equal(object = s$iknots[[9L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.809851868675398))
            expect_equal(object = s$iknots[[10L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.678991596638655))
            expect_equal(object = s$iknots[[11L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.699049316696375))
            expect_equal(object = s$iknots[[12L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.646646894051738))
            expect_equal(object = s$iknots[[13L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.592728758169935))
            expect_equal(object = s$iknots[[14L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.539154539154539))
            expect_equal(object = s$iknots[[15L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.518443392682744))
            expect_equal(object = s$iknots[[16L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.459785577432636))
            expect_equal(object = s$iknots[[17L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.352286542597961))
            expect_equal(object = s$iknots[[18L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.320280112044818))
            expect_equal(object = s$iknots[[19L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.299470058293588))
            expect_equal(object = s$iknots[[20L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.189492013021425))
            expect_equal(object = s$iknots[[21L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.959346405228758))
            expect_equal(object = s$iknots[[22L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.161822999470058))
            expect_equal(object = s$iknots[[23L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.111297852474323))
            expect_equal(object = s$iknots[[24L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.0398190045248869))
            expect_equal(object = s$iknots[[25L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.0132485426603076))
            expect_equal(object = s$iknots[[26L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.27128197716433))
            expect_equal(object = s$iknots[[27L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.914066496163683))
            expect_equal(object = s$iknots[[28L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.758067490319012))
            expect_equal(object = s$iknots[[29L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.433121356650768))
            expect_equal(object = s$iknots[[30L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.48081336238199))
            expect_equal(object = s$iknots[[31L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.216953526416442))
            expect_equal(object = s$iknots[[32L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, 0.910716158121002))
            expect_equal(object = s$iknots[[33L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, 0.77688651218063))
            expect_equal(object = s$iknots[[34L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, 0.847617780143732))
            expect_equal(object = s$iknots[[35L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, 0.709507332344702))
            expect_equal(object = s$iknots[[36L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, 0.636618283677107))
            expect_equal(object = s$iknots[[37L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.78242220801364))
            expect_equal(object = s$iknots[[38L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.62116477532299))
            expect_equal(object = s$iknots[[39L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.406681190994917))
            expect_equal(object = s$iknots[[40L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, 0.213106295149639))
            expect_equal(object = s$iknots[[41L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, 0.278400248988485))
            expect_equal(object = s$iknots[[42L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, 0.354918437963421))
            expect_equal(object = s$iknots[[43L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, 0.4159938485198))
            expect_equal(object = s$iknots[[44L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.084920634920635))
            expect_equal(object = s$iknots[[45L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, 0.576293675364883))
            expect_equal(object = s$iknots[[46L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.24139979920913))
            expect_equal(object = s$iknots[[47L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, 0.144092931290163))
            expect_equal(object = s$iknots[[48L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.727465433347786))
            expect_equal(object = s$iknots[[49L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, -0.566268826371128))
            expect_equal(object = s$iknots[[50L]], expected = expected_iknots)
            expected_iknots <- sort(c(expected_iknots, 0.480392156862745))
            expect_equal(object = s$iknots[[51L]], expected = expected_iknots)
          }
)


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
###   e2 <- substitute(expect_equal(object = s$iknots[[ii]], expected = expected_iknots), list(ii = i))
###   print(e1)
###   eval(e1)
###   print(e2)
### }
###
################################################################################
###                               End of File                                ###
################################################################################

