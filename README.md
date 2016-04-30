# CPR: Control Polygon Reduction
This is an R package for implementing the Control Polygon Reduction model
selection method.  When we are tasked with modeling the functional relationship
between a response and a continuous predictor, i.e., y = f(x), CPR allows for quick and
efficient searching of a large model space to find B-spline estiamtes of the
function f(x).  

## Learn More About CPR.
This model selection method was developed as part of Peter DeWitt's PhD
dissertation work.  Please refer to the vignettes for more details on the
different parts of the package.

```r
vignette(package = 'cpr')
```

## Installing CPR
Currently the CPR is not public.  If you are reading this README file you must
have opened the tar ball.  Install from the command line

```
R CMD INSTALL cpr_<version>.tar.gz
```

## 

