# Version 0.4.1
* Initial Submission 23 Dec 2025

## CRAN Package Check Notes:

There are two notes on: https://cran.r-project.org/web/checks/check_results_cpr.html

r-oldrel-macos-arm64, and r-oldrel-macos-x86_64 both report

    Version: 0.4.0
    Check: installed package size
    Result: NOTE
        installed size is  5.3Mb
        sub-directories of 1Mb or more:
          doc    1.6Mb
          libs   2.9Mb

The issue here is the libs sub-directory.  This is not an issue with other OS
and is a result of the compiler used.

## R CMD check results

* Local (macOS)
  * R-4.5.2 --as-cran status ok

* GitHub actions:
  * mac os R-release (R-4.5.2): status ok
  * windows R-release: status ok
  * ubuntu R-devel: status ok
  * ubuntu R-release: status ok
  * ubuntu R-oldrel-1:  status ok

* win builder (R-devel)
  * status ok

* rhub
  * status ok for the checks that were able to install dependencies

## Reverse dependencies

* None
## Reverse suggests

* None

# Version 0.4.0
* Updating from v0.3.0 to v0.4.0
* Initial submission 14 Feb 2024

## R CMD check results

* Github Actions:
  * macOS R-4.3.2
  * windows R-4.3.2
  * ubuntu R-4.3.2
  * ubuntu R-devel
  * ubuntu R-4.2.3

* rhub

  * Windows Server 2022; R-devel
    - * checking package dependencies ... ERROR
      Package required and available but unsuitable version: 'Matrix'

    This is a potential problem that having a version number associated with
    Matrix fixes.  There was a breaking change in Matrix 1.6-2 which impacts the
    package lme4.  To insure that lme4 works as expected, and thus the cpr
    package, then Matrix needs to be of a sufficient version.  Note that checks
    on other windows systems end in Status OK


* win-builder.r-project.org

  * R-devel
    - Status OK

  * R version 4.3.2
    - Status OK

* Local (macOS Monterey 12.6)
  * R 4.3.2

## Reverse dependencies

    None


## Reverse suggests

    None
