library(cpr)

A <- matrix(1:4, nrow = 10, ncol = 20)
B <- matrix(1:6, nrow = 10, ncol = 6)

# verify that passing in one matrix returns that matrix
x <- tryCatch(build_tensor(A, 1:10), error = function(e) e)
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "All arguments passed to build_tensor need to be matrices."))

# verify that passing in one matrix returns that matrix
tA <- build_tensor(A)
stopifnot(identical(tA, A))

# Two ways of building the same tensor product
tAB  <- build_tensor(A, B)
tAB2 <- do.call(build_tensor,list(A, B))
stopifnot(all.equal(tAB, tAB2))

# direction matters
tBA <- build_tensor(B, A)
stopifnot(!isTRUE(all.equal(tBA, tAB)))

# three and four matrix tensor product
tABA  <- build_tensor(A, B, A)
tABAA <- build_tensor(A, B, A, A)

# verify to established matrices
rdsAB   <- readRDS("tensorAB.rds")
rdsBA   <- readRDS("tensorBA.rds")
rdsABA  <- readRDS("tensorABA.rds")
rdsABAA <- readRDS("tensorABAA.rds")

stopifnot(identical(tAB, rdsAB))
stopifnot(identical(tBA, rdsBA))
stopifnot(identical(tABA, rdsABA))
stopifnot(identical(tABAA, rdsABAA))


################################################################################
#                                 End of File                                  #
################################################################################
