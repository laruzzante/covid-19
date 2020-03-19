BASE_DIR <- "~/covid-19/"
setwd(BASE_DIR)

source("data.R")
source("plots.R")

data <- get_data()

countries <- c("Italy", "Spain", "Switzerland", "France",
               "Germany", "United Kingdom", "Netherlands",
               "Norway", "Sweden", "South Africa")

# Plot data for a single country
# plot_new_cases(data, countries = "Switzerland", metric = "confirmed")

# Plot data for multiple countries
# plot_new_cases(data, countries = Countries, logtrans = FALSE, fit_line = FALSE)

# Plot all metrics (cases, deaths, recovered) for multiple countries
plot_all_metrics_cases(data, countries)
plot_all_metrics_cases(data, countries, new_cases_only = TRUE)
