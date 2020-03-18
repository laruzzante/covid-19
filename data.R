library(lubridate)
library(RCurl)
library(readr)

BASE_REPO_URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/"
BASE_DATA_URL <- paste0(BASE_REPO_URL, "csse_covid_19_data/csse_covid_19_time_series/")

CONFIRMED_REPORT_URL <- paste0(BASE_DATA_URL, "time_series_19-covid-Confirmed.csv")
DEATH_REPORT_URL <- paste0(BASE_DATA_URL, "time_series_19-covid-Deaths.csv")
RECOVERED_REPORT_URL <- paste0(BASE_DATA_URL, "time_series_19-covid-Recovered.csv")


# Get data from John Hopkiins' github repo
get_data <- function() {

    confirmed_data <- read_csv(CONFIRMED_REPORT_URL)
    death_data <- read_csv(DEATH_REPORT_URL)
    recovered_data <- read_csv(RECOVERED_REPORT_URL)

    data <- list(confirmed = confirmed_data, deaths = death_data, recovered = recovered_data)

    return(data)

}



# Get data for a single or a list of countries
get_country_data <- function(data, country, metric = "confirmed", mainland = TRUE) {

    if (!is.vector(country)) country <- c(country)

    data <- data[[metric]]


    for (i in 1:length(country)) {
        if (!(country[i] %in% data$"Country/Region")) {
            warning(paste0("Specified country <", country[i], "> not found in data"))
        }
    }

    data <- data[data$"Country/Region" %in% country, ]
    data <- melt(data, id.vars = c("Province/State", "Country/Region", "Lat", "Long"),
                 variable.name = "Date", value.name = "Cases")

    names(data) <- c("State", "Country", "Lat", "Long", "Date", "Cases")

    data$Date <- mdy(data$Date)

    data <- data[order(data$"Country", data$"State", data$Date), ]

    if (mainland == TRUE) {

        data <- subset(data, is.na(data$"State") | data$"State" %in% country)

    }

    return(data)

}


# Get new cases data for a single or a list of countries
get_country_data_new_cases <- function(data, country, metric = "confirmed", mainland = TRUE) {
  
  data <- get_country_data(data, country, metric)
  
  if (!is.vector(country)) country <- c(country)
  
  data_new_cases <- c()
  
  for(country in countries){
    
    country_data_subset <- subset(data, Country==country)
    rownames(country_data_subset) <- 1:nrow(country_data_subset)
    country_data_subset$New <- country_data_subset$Cases
    
    for (i in 1:nrow(country_data_subset)) {
      
      if(i == 1) { country_data_subset[i, 'New'] <- country_data_subset[i, 'Cases']
      } else {
        country_data_subset[i, 'New'] <- country_data_subset[i, 'Cases'] - country_data_subset[i-1, 'Cases']
      }
    }
    
    data_new_cases <- rbind(data_new_cases, country_data_subset)
  }
  return(data_new_cases)
  
}
