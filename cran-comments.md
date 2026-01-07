# Version 0.4.1

* Initial Submission 7 Jan 2026

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
