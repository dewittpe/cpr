# Version 0.3.0
- Updating from v0.2.3 to v0.3.0
- Initial submission 29 November 2023

## R CMD check results

* Github Actions:
  * macOS R-4.3.2
  * windows R-4.3.2
  * ubuntu R-4.3.2
  * ubuntu R-devel
  * ubuntu R-4.2.3

* rhub

* win-builder.r-project.org

* Local (macOS Monterey 12.6)
  * R 4.3.2

All pass with Status OK with one caveat:

In some cases there was an error thrown as a result of lme4 being built againt
Matrix < v1.6-2 while other packages where built against Matrix >= 1.6-2.
Because lme4 is used in examples, and that was the cause of the error, I have
moved the examples that could error into dontrun sections for this release.


## Reverse dependencies

    None

## Reverse suggests

    None
