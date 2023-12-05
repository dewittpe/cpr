# CPR Package data-raw

## SWAN Data
There are two files needed for this data set build:
    1. `dhs_summary.rds` and
    2. `spdg.R`.

The first file, `dhs_summary.rds` is a list object with summary statistics and
densities based on a subset of the Study of Women's Health Across the Nation
(SWAN) Daily Hormone Study (DHS) data set.   The `spdg.R` file creates the data
set `spdg` which is provided in the `cpr` package to illustrate the basic use of
the package.

## Laboratory-confirmed COVID-19 Cases

Raw data was downloaded from:

https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf

Last download was 5 December 2023.

The data is large ~11GB, and thus is not part of the git repository.

The data is summarized and outputed as the number of laboratory-confirmed
COVID-19 cases by day from January 1 2020 through May 11 2023, the last day of
the public health emergency declaration.
