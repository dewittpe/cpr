# cpr: Control Polygon Reduction <img src="/man/figures/cpr-hex-small.png" align="right"/>

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/dewittpe/cpr/workflows/R-CMD-check/badge.svg)](https://github.com/dewittpe/cpr/actions)
[![Coverage Status](https://img.shields.io/codecov/c/github/dewittpe/cpr/master.svg)](https://app.codecov.io/github/dewittpe/cpr?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/cpr)](https://cran.r-project.org/package=cpr)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/cpr)](http://www.r-pkg.org/pkg/cpr)

An R package for implementing the Control Polygon Reduction model
selection method.  When we are tasked with modeling the functional relationship
between a response and a continuous predictor, i.e., y = f(x), CPR allows for quick and
efficient searching of a large model space to find B-spline estimates of the
function f(x).

CPR extends to multiple dimensions and allows one to find good locations for
knots in a tensor product of B-splines.

## Learn More About CPR.
This model selection method was developed as part of [Peter DeWitt's PhD
dissertation](https://doi.org/10.25677/awnc-b795) work.  It has been extended
since.

### Vignettes
There are two vignettes within the package
```r
vignettes(topic = 'cpr', package = 'cpr')
vignettes(topic = 'cnr', package = 'cpr')
```

You can also read these on package website


### Related Publications:

* The CPR method was presented at the 28th International Biometric Conference
  held July 2016, in Victoria, British Columbia, Canada.  The abstract, paper,
  and talk had the title:
  "Parsimonious B-splines Regression Models via Control Polygon
  Reduction."  A bibtex entry for the abstract:

```
@inproceedings{,
  author       = {DeWitt, Peter E. and Carlson, Nichole E. and Samantha MaWhinney},
  title        = {Parsimonious B-spline Regression Models via Control Polygon Reduction},
  booktitle    = {Abstracts for the XXVIIIth International Biometric Conference},
  month        = {July},
  year         = {2016},
  organization = {International Biometric Society},
  address      = {Victoria, British Columbia},
  url          = {http://www.biometricsociety.org/conference-abstracts/2016/},
  isbn         = {978-0-9821919-4-1}
}
```

**Awards:** The presentation of the work earned Peter DeWitt two awards:

1. "Best Student Oral Presentation" from the International Biometric Society
   (IBS).
2. "Distinguished Oral Presentation" as part of the student
   paper competition hosted by the Western North American Region
   (WNAR) of the IBS.

## Installing CPR
Options for installing CPR:
1. Install from the Comprehensive R Archive Network (CRAN)

```
# within R
install.packages("cpr", repos = "https://cran.rstudio.com")
```

2. Install the developmental version from github.  This will require you to have
   [remotes](https://cran.r-project.org/package=remotes) installed, and, if you are
   using Windows, you'll need
   [Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed as well.

```
remotes::install_github("dewittpe/cpr", build_vignettes = TRUE)
```

3. Clone the repository and use `GNU make`

```bash
make install
```

4. Go to the [release page](https://github.com/dewittpe/cpr/releases) and down
   load the `cpr_<version>.tar.gz` file of the version you want to install.

  * Install from the command line

```bash
R CMD INSTALL cpr_<version>.tar.gz
```

  * Within R

```r
install.packages(<path_to_file>, repos = NULL, type="source")
```

## Other Notes:
The `cpr` package provides 3D graphics via the
[`rgl`](https://CRAN.R-project.org/package=rgl) package.  If you are get an
error, or rather message, of the form:
```
  font family "sans" not found, using "bitmap"
```
Then there is an easy fix.  You need to get the [FreeType 2 font
engine](https://freetype.org/).

On Debian, you can get the library via:

```
apt-get install libfreetype6-dev
```

Once the FreeType 2 font engine has been installed on your computer you **must
reinstall** the `rgl` package.
