library(cowplot)
library(ggplot2)
library(reshape2)


# Top-level function to plot all metrics in a pdf file
plot_all_metrics <- function(data, countries) {
  
  plot_list <- list()
  metrics <- c("confirmed", "deaths", "recovered")
  i <- 0
  
  for (metric in metrics) {

    for (logtrans in c(FALSE, TRUE)) {
      
      i <- i + 1
      
      p <- plot_new_cases(data, countries = countries,
                           metric = metric, fit_line = TRUE, logtrans = logtrans)
      plot_list[[i]] <- p
      
    }
  }
  
  glist <- lapply(plot_list, ggplotGrob)
  multi.page <- ggpubr::ggarrange(plotlist = glist, nrow = 2, ncol = 1) 
  ggpubr::ggexport(multi.page, filename = "covid-19-total-cases.pdf")
  
}



# High-level function to plot data over time for several countries directly from global data
plot_new_cases <- function(data, countries, metric = "confirmed", logtrans = FALSE, fit_line = FALSE) {

    data <- get_country_data(data, countries, metric = metric)
    
    new_cases <- draw_new_cases(data, metric = metric, logtrans = logtrans, fit_line = fit_line)

    return(new_cases)

}



# Low level function to generate time series plot for multiple countries
draw_new_cases <- function(data, metric = "confirmed", logtrans = FALSE, fit_line = FALSE) {

    y_axis_title <- paste0("Number of ", metric)
    if (metric %in% c("recovered", "confirmed")) y_axis_title <- paste0(y_axis_title, " cases")
    if (logtrans == TRUE) y_axis_title <- paste0(y_axis_title, " (log2)")


    g <- ggplot(data, aes(x = Date, y = Cases, color = Country, fill = Country)) +
        geom_point(size = 2) +
        theme_cowplot() +
        theme_set(theme_classic(base_size = 10))+
        theme(axis.title.x = element_blank(),
              legend.position = c(0.1, 0.66)) +
        ylab(y_axis_title)

    if (logtrans == TRUE) g <- g + scale_y_continuous(trans = "log2")

    if (fit_line == TRUE) g <- g + geom_line(size = 1)

    return(g)

}
