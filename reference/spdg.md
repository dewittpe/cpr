# Simulated Pregnanediol glucuronide (PDG) Data

A Simulated data set based on the Study of Women's Health Across the
Nation (SWAN) Daily Hormone Study (DHS).

## Usage

``` r
spdg
```

## Format

a `data.frame`. Variables in the data set:

- id:

  Subject ID

- age:

  Age, in years of the subject

- ttm:

  Time-to-menopause, in years

- ethnicity:

  Ethnicity, a factor with five levels: Caucasian, Black, Chinese,
  Hispanic, and Japanese

- bmi:

  Body Mass Index

- day_from_dlt:

  A integer value for the number of days from Day of Luteal Transition
  (DLT). The DLT is `day_from_dlt == 0`. Negative values indicate the
  follicular phase, positive values for the luteal phase.

- day_of_cycle:

  the day of cycle

- day:

  A scaled day-of-cycle between \[-1, 1\] with 0 for the DLT. See
  Details

- pdg:

  A simulated PDG value

## Source

This is simulated data. To see the script that generated the data set
please visit <https://github.com/dewittpe/cpr> and look at the scripts
in the data-raw directory.

## Details

Pregnanediol glucuronide (PDG) is the urine metabolite of progesterone.
This data set was simulated to have similar characteristics to a subset
of the SWAN DHS data. The SWAN DHS data was the motivating data set for
the method development that lead to the `cpr` package. The DHS data
cannot be made public, so this simulated data set has been provided for
use in examples and instructions for use of the `cpr` package.

## References

Santoro, Nanette, et al. "Body size and ethnicity are associated with
menstrual cycle alterations in women in the early menopausal transition:
The Study of Women's Health across the Nation (SWAN) Daily Hormone
Study." The Journal of Clinical Endocrinology & Metabolism 89.6 (2004):
2622-2631.
