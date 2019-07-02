#' calc_month_anom
#'
#' @param df dataframe of monthly means or max
#'
#' @return dataframe that has anomaly averaged from each month across years
#' @export
#'
#' @examples

calc_month_anom = function(df){
  
    df_month = df %>%
    mutate(month = month(date))  
  
  intra_month = df_month %>% 
    group_by(month) %>% 
    summarize(mean_swe = mean(swe_mm))
  
  intra_month_anomaly = merge(df_month, intra_month, by = "month")%>%
    mutate(anomaly = swe_mm - mean_swe) %>% 
    mutate(sign = ifelse(anomaly < 0, "negative", "positive") )
  
  return(intra_month_anomaly = intra_month_anomaly)
}