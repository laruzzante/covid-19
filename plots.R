library(cowplot)
library(ggplot2)
library(reshape2)


# Top-level function to plot all metrics in a pdf file
plot_all_metrics_cases <- function(data, countries, new_cases_only = FALSE) {
  
  plot_list <- list()
  metrics <- c("confirmed", "deaths", "recovered")
  i <- 0
  
  for (metric in metrics) {
    
    for (logtrans in c(FALSE, TRUE)) {
      
      i <- i + 1
      
      p <- plot_cases(data, countries = countries,
                          metric = metric, fit_line = TRUE, logtrans = logtrans, new_cases_only = new_cases_only)
      plot_list[[i]] <- p
      
    }
  }
  
  glist <- lapply(plot_list, ggplotGrob)
  multi.page <- ggpubr::ggarrange(plotlist = glist, nrow = 2, ncol = 1) 
  if(new_cases_only == TRUE) {
    ggpubr::ggexport(multi.page, filename = "figures/new-cases.pdf")
  } else ggpubr::ggexport(multi.page, filename = "figures/total-cases.pdf")
  
  
}


# High-level function to plot data over time for several countries directly from global data
plot_cases <- function(data, countries, metric = "confirmed", logtrans = FALSE, fit_line = TRUE, new_cases_only = FALSE) {

    if (new_cases_only == TRUE) {
      data <- get_country_data_new_cases(data, countries, metric = metric)
    } else data <- get_country_data(data, countries, metric = metric)
    
    new_cases <- draw_cases(data, metric = metric, logtrans = logtrans, fit_line = fit_line, new_cases_only = new_cases_only)

    return(new_cases)

}


# Low level function to generate time series plot for multiple countries
draw_cases <- function(data, metric = "confirmed", logtrans = FALSE, fit_line = FALSE, new_cases_only = FALSE) {
  
  if (new_cases_only == TRUE) {
    y_axis_title <- paste0("Number of new ", metric)
  } else y_axis_title <- paste0("Number of ", metric)
  
  if (metric %in% c("recovered", "confirmed")) y_axis_title <- paste0(y_axis_title, " cases")
  if (logtrans == TRUE) y_axis_title <- paste0(y_axis_title, " (log2)")
  
  if (new_cases_only == TRUE)  {
    g <- ggplot(data, aes(x = Date, y = New, color = Country, fill = Country))
  } else g <- ggplot(data, aes(x = Date, y = Cases, color = Country, fill = Country))
    
  g <- g + geom_point(size = 1) +
      theme_cowplot() +
      theme_set(theme_classic(base_size = 10))+
      theme(axis.title.x = element_blank(),
            legend.position = c(0.1, 0.6)) +
      ylab(y_axis_title)
  
  if (logtrans == TRUE) g <- g + scale_y_continuous(trans = "log2")
  
  if (fit_line == TRUE) g <- g + geom_line(size = 0.5)
  
  return(g)
  
}
