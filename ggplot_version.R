BASE_DIR <- "~/covid-19/"
setwd(BASE_DIR)

source("data.R")
source("plots.R")

data <- get_data()

# Plot data for a single country
plot_new_cases(data, countries = "Switzerland", metric = "confirmed")

# Plot data for multiple countries
plot_new_cases(data, countries = c("Italy", "Spain", "Switzerland", "France", "Germany", "United Kingdom"),
               metric = "confirmed", fit_line = TRUE)

