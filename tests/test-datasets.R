library(cpr)

################################################################################
stopifnot("available data sets" =
  identical(
    data(package = "cpr")$results[, "Item"]
    ,
    c("spdg", "us_covid_cases")
  )
)

################################################################################
#                        simple checks of the spdg data                        #
stopifnot(identical(class(spdg), "data.frame"))
stopifnot(identical(dim(spdg), c(24628L, 9L)))
stopifnot(identical(names(spdg), c("id", "age", "ttm", "ethnicity", "bmi", "day_from_dlt", "day_of_cycle", "day", "pdg")))

################################################################################
#                       simple checks of us_covid_cases                        #
stopifnot(identical(class(us_covid_cases), "data.frame"))
stopifnot(identical(dim(us_covid_cases), c(1227L, 2L)))
stopifnot(identical(names(us_covid_cases), c("date", "cases")))

################################################################################
##                                End of File                                 ##
################################################################################
