
# Gather data -------------------------------------------------------------

# Ref: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data

library(tidyverse)
library(jsonlite)
library(leaflet)
library(ggmap)
library(janitor)
library(lubridate)
covid_death <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
covid_recoveries <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
covid_confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")


convert_to_long <- function(df = covid_death, val_name = "deaths"){
    df %>%
        clean_names(case = 'snake') %>%
        pivot_longer(cols = starts_with('x'),
                     names_to = "date", values_to = val_name) %>%
        mutate(date = mdy(gsub('x','', date)))
}

deaths_long <- convert_to_long(covid_death, "deaths")
recoveries_long <- convert_to_long(covid_recoveries, "recoveries")
confirmed_long  <- convert_to_long(covid_confirmed, "confirmed")

covid_master <- confirmed_long %>%
    full_join(deaths_long, by = c("date","province_state", "country_region","lat","long")) %>%
    full_join(recoveries_long, by = c("date", "province_state", "country_region","lat","long"))

# Plot totals
covid_master %>%
    group_by(date) %>%
    summarise(confirmed = sum(confirmed, na.rm = TRUE),
              deaths = sum(deaths, na.rm = TRUE),
              recoveries = sum(recoveries, na.rm = TRUE)) %>%
    pivot_longer(-date, names_to = "outcome", values_to = "total") %>%
    ggplot(aes(x = date, y = total, colour = outcome)) +
        geom_line() + geom_point() +
        theme_minimal() +
        theme(legend.position = 'bottom') +
        labs(title = "Coronavirus global outcomes - 2020",
             x = "Date", y = "Cumulative total", color = "Outcomes") +
        scale_colour_manual(values = c("orange2", 'red3', "green4"))
