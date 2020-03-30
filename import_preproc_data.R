
# Gather data -------------------------------------------------------------

# Ref: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
# Setup functions ------------------------------------------------------------
get_package <- function(package){
    if (!package %in% installed.packages()){
        install.packages(package, repos = "http://cran.rstudio.com/")
    }
    invisible(library(package, character.only = TRUE))
}


packages_required <- c("tidyverse", "jsonlite", "sf", "tmaptools", "janitor",
                       "leaflet", "maps", "ggmap", "ggthemes", "gganimate","lubridate")
sapply(packages_required, get_package)

covid_death <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv')
covid_recoveries <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv')
covid_confirmed <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')

# OLD URLS --------
# read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
# read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
# read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
# -----------------
# covid_daily <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-29-2020.csv")


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

# Plot totals ---------------------------
## Global
global_tot <- covid_master %>%
    group_by(date) %>%
    summarise(confirmed = sum(confirmed, na.rm = TRUE),
              deaths = sum(deaths, na.rm = TRUE),
              recoveries = sum(recoveries, na.rm = TRUE)) %>%
    pivot_longer(-date, names_to = "outcome", values_to = "total")



## Regional
regional_tot <- covid_master %>%
    group_by(date, province_state, country_region, lat, long) %>%
    summarise(confirmed = sum(confirmed, na.rm = TRUE),
              deaths = sum(deaths, na.rm = TRUE),
              recoveries = sum(recoveries, na.rm = TRUE)) %>%
    pivot_longer(cols = c(confirmed, deaths, recoveries),
                 names_to = "outcome", values_to = "total")

