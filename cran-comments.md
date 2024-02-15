# Version 0.4.0
- Updating from v0.3.0 to v0.4.0
- Initial submission 14 Feb 2024

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
