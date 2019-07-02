#' plot_swe_anomaly
#' 
#' Plots the anomaly of the dataframe, not including 0 values
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples

plot_swe_anomaly = function(df, region){
  
   # anomaly_df = df %>% 
    #mutate(swe_w_na = na_if(swe_mm, 0)) %>% this is if we want to take out the zeros
    #mutate(anomaly = swe_w_na - mean(swe_w_na, na.rm = TRUE)) %>% 
   # mutate(anomaly = swe_mm - mean(swe_mm)) %>% 
    # mutate(sign = ifelse(anomaly < 0, "negative", "positive") )
  
  plot = df %>% 
    ggplot(aes(x = date, y = anomaly)) +
    geom_col(aes(fill = sign), show.legend = FALSE) +
    #scale_y_continuous(limits = c(-20, 100)) +
    scale_fill_manual(values = c("negative" = "red", "positive" = "dark green")) +
    scale_x_date(date_breaks = "2 years") +
    labs(
      x = "Date",
      y = "Anomaly",
      title = sprintf("%s", region)
    ) +
    theme_classic()
  
  
  return(anomaly_plot = plot)
}