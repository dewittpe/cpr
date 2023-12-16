library(data.table)
us_covid_cases <- fread("~/Downloads/COVID-19_Case_Surveillance_Public_Use_Data.csv")

us_covid_cases <- us_covid_cases[current_status == "Laboratory-confirmed case", .N, keyby = .(cdc_case_earliest_dt)]
us_covid_cases[, date := as.IDate(cdc_case_earliest_dt) ]
us_covid_cases <- subset(us_covid_cases, date <= "2023-05-11") # end of the public health emergency declaration

#us_covid_cases[, wday := weekdays(date)]
#us_covid_cases[, wday := factor(wday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))]
#
#ggplot2::ggplot(us_covid_cases) +
#ggplot2::aes(x = date, y = N) +
#ggplot2::geom_path() +
#ggplot2::scale_x_date(date_breaks = "3 months",
#                      date_minor_breaks = "1 months",
#                      date_labels = "%b\n%Y") +
#ggplot2::scale_y_continuous(name = "Laboratory-confirmed cases", labels = scales::comma) +
#ggplot2::theme(axis.title.x = ggplot2::element_blank())

us_covid_cases <- us_covid_cases[, .(date = as.Date(date), cases = N)]

us_covid_cases <- as.data.frame(us_covid_cases)

save(us_covid_cases, file = "../data/us_covid_cases.rda")
