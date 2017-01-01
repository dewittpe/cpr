library(Matrix)
library(Rcpp)
library(RcppArmadillo)
library(rbenchmark)
library(cpr)

Mat <- matrix(rnorm(25), nrow = 5)



cppFunction(depends = "RcppArmadillo",
            'double matrank(NumericMatrix m) {
              arma::mat X(m.begin(), m.nrow(), m.ncol(), false);
              return arma::rank(X);
            }')


benchmark(rankMatrix(Mat), matrank(Mat))

xvec <- runif(25e3)
Mat <- splines::bs(xvec,    knots = seq(0.1, 0.9, length = 51), Boundary.knots = c(0, 1), intercept = TRUE)

rankMatrix(Mat, method = "tolNorm2")[1]
rankMatrix(Mat, method = "qr.R")[1]
rankMatrix(Mat, method = "qrLINPACK")[1]
rankMatrix(Mat, method = "qr")[1]
rankMatrix(Mat, method = "useGrad")[1]
rankMatrix(Mat, method = "maybeGrad")[1]
matrank(Mat)

benchmark(
rankMatrix(Mat, method = "tolNorm2"),
rankMatrix(Mat, method = "qr.R"),
rankMatrix(Mat, method = "qrLINPACK"),
rankMatrix(Mat, method = "qr"),
rankMatrix(Mat, method = "useGrad"),
rankMatrix(Mat, method = "maybeGrad"),
matrank(Mat)
)
