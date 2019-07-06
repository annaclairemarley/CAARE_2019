#' graph_swe_metric
#' 
#' Graphs your swe metrics
#'
#' @param df the dataframe to display. Expects daata to display to be in column swe_mm and date axis to be named date. Should also contain column called waterYear
#' @param title Title of plot
#' @param type type of geom plot. examples are geom_line, geom_point, geom_col etc
#'
#' @return visualization of data
#' @export
#'
#' @examples
#' 
graph_with_wateryear = function(df, title = "", type=geom_line, variable = "swe_mm", ylab = "SWE (mm)") {
  
  # calculate start and end water year for graphing
  startWaterYear = min(df$waterYear)
  endWaterYear = max(df$waterYear)
  
  # Create sequence of real dates, on which to place the water year lables
  breakDates = seq.Date(from = ymd(paste(startWaterYear,1,1, sep='-')), length.out = endWaterYear - startWaterYear + 1, by = "1 year")
  
  swe_daily_graph = df %>% 
    ggplot(aes_string("date", variable)) +
    type() +
    scale_x_date(date_breaks = "3 month", date_labels="%b", name="Month",
                 sec.axis = sec_axis(~. , name="Water Year",  breaks= breakDates, labels=seq(startWaterYear,endWaterYear,by=1))
    ) +
    scale_y_continuous(expand = c(0,0)) +
    labs(
      x = "Month",
      y = ylab,
      title = sprintf("%s", title)
    ) +
    theme_classic()
  
  return(swe_daily = swe_daily_graph)
  
}

#####################################################################
#####################################################################

#' plot_swe_anomaly
#' 
#' Plots the anomaly of the dataframe. Best if input the monthly mean dataframe into this as it
#' will calculate the mean of all the monthly means and then make an anomaly
#'
#' @param df monthly mean dataframe
#' @param title put the region in quotes like title = "Carrizo" to label the graph
#'
#' @return Anomaly graph 
#' @export
#'
#' @examples

plot_swe_anomaly = function(df, title = ""){
  
  startWaterYear = min(df$waterYear)
  endWaterYear = max(df$waterYear)
  
  # Create sequence of real dates, on which to place the water year lables
  breakDates = seq.Date(from = ymd(paste(startWaterYear,1,1, sep='-')), length.out = endWaterYear - startWaterYear + 1, by = "1 year")
  
  plot = df %>% 
    ggplot(aes(x = date, y = anomaly)) +
    geom_col(aes(fill = sign), show.legend = FALSE) +
    #scale_y_continuous(limits = c(-20, 100)) +
    scale_fill_manual(values = c("negative" = "red", "positive" = "dark green")) +
    scale_x_date(date_breaks = "3 month", date_labels="%b", name="Month",
                 sec.axis = sec_axis(~. , name="Water Year",  breaks= breakDates, labels=seq(startWaterYear,endWaterYear,by=1))) +
    labs(
      x = "Date",
      y = "Anomaly",
      title = sprintf("%s", title)
    ) +
    theme_classic()
  
  
  return(anomaly_plot = plot)
}

#####################################################################
#####################################################################

#' plot_swe_month
#' 
#' Plots the monthly metric of each region, 
#'
#' @param df monthly metric dataframe of each region already narrowed down to month you want using month_ts
#'
#' @return  
#' @export
#'
#' @examples plot_swe_month(carr_monthly, ch_monthly, bm_monthly)

plot_months = function(df, month = "") {

  plot = df %>% 
  ggplot(aes(x = waterYear, y = swe_mm, fill = region)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_x_continuous(breaks=seq(2004,2019, by = 1)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Water Year",
       y = "SWE (mm)",
       title = sprintf("SWE %s Month``", month)
       ) +
  theme_classic()

  return(plot)

}


