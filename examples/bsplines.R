# build a vector of values to transform
xvec <- seq(-3, 5, length = 100)

# cubic b-spline
bmat <- bsplines(xvec, iknots = c(-2, 0, 1.2, 1.2, 3.0))
bmat

# plot the splines
plot(bmat)                # each spline will be colored by default
plot(bmat, color = FALSE) # black and white plot
plot(bmat, color = FALSE) + ggplot2::aes(linetype = spline) # add a linetype

# Axes
# The x-axis, by default, show the knot locations.  Other options are numeric
# values, and/or to use a second x-axis

plot(bmat, show_xi = TRUE,  show_x = FALSE) # default, knot, symbols, on lower axis
plot(bmat, show_xi = FALSE, show_x = TRUE)  # Numeric value for the knot locations
plot(bmat, show_xi = TRUE,  show_x = TRUE)  # symbols on bottom, numbers on top

# quadratic splines
bmat <- bsplines(xvec, iknots = c(-2, 0, 1.2, 1.2, 3.0), order = 3L)
bmat
plot(bmat) + ggplot2::ggtitle("Quadratic B-splines")
