# CPR: Control Polygon Reduction

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.com/dewittpe/cpr.svg?token=jowN3QrQKY7UdmTc4Efp&branch=master)](https://travis-ci.com/dewittpe/cpr)

An R package for implementing the Control Polygon Reduction model
selection method.  When we are tasked with modeling the functional relationship
between a response and a continuous predictor, i.e., y = f(x), CPR allows for quick and
efficient searching of a large model space to find B-spline estiamtes of the
function f(x).  

CPR extends to multiple dimensions and allows one to find good locations for
knots in a tensor product of B-splines.

## Learn More About CPR.
This model selection method was developed as part of Peter DeWitt's PhD
dissertation work.  

### Vignettes

Please refer to the vignettes for more details on the different parts of the package.

```r
# See all vignettes provided in the `cpr` packge
vignette(package = 'cpr')
```

### Related Publications:

* The CPR method was presented at the 28th International Biometric Conference
  held July 2016, in Victoria, British Columbia, Canada.  The abstract, paper,
  and talk had the title: 
  "Parsimonious B-splines Regression Models via Control Polygon
  Reduction."  A bibtex entry for the abstract:

    @inproceedings{,
      author = {DeWitt, Peter E. and Carlson, Nichole E. and Samantha MaWhinney},
      title = {Parsimonious B-spline Regression Models via Control Polygon Reduction},
      booktitle = {Abstracts for the XXVIIIth International Biometric Conference}, 
      month = {July}, 
      year  = {2016}, 
      organization = {International Biometric Society}, 
      address = {Victoria, British Columbia},
      url = {http://www.biometricsociety.org/conference-abstracts/2016/},
      isbn = {978-0-9821919-4-1}
    }

**Awards:** The presentation of the work earned Peter DeWitt two awards:

1. "Best Student Oral Presentation" from the International Biometric Society
   (IBS).
2. "Distinguished Oral Presentation" as part of the student
   paper competition hosted by the Western North American Region
   (WNAR) of the IBs.

## Installing CPR
Currently CPR is under private development.  

Options for installing CPR:

1. Clone the repo and use `GNU make`

    make install

2. Go to the [release page](https://github.com/dewittpe/cpr/releases) and down
   load the tar.gz file of the version you want to install.

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
engine](https://www.freetype.org/).  

On Debian, you can get the library via:

```
apt-get install libfreetype6-dev
```

Once the FreeType 2 font engine has been installed on your computer you **must
reinstall** the `rgl` package.
