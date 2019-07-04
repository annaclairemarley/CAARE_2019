


#' extract_winter_months
#' 
#' CExtracts winter months and standardises column names
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples

extract_winter_months = function (df) {
  filteredDF <- df %>% 
    rename(date = date_time, swe_mm = SWE_mm) %>% 
    filter(month(date) %in% c(11, 12, 1, 2, 3, 4))
  return(filteredDF)
}

#' add_water_year
#' 
#' Changes the date column to water year
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples

add_water_year = function(df) {
  
  wy_df = df %>% 
    dplyr:: rename(Date = date) %>%
    dataRetrieval::addWaterYear() %>%
    dplyr:: rename(date = Date)
  
  return(wy_df)
}

#' calc_swe_metrics
#'
#' Makes a dataframe of the summary statistic over the time period you want
#'
#' @param region 
#' @param period the time period you want to group by; enter "month", "year", or "1 week" 
#'       **make sure you put the period you want in quotes!!!**
#' @param statistic the summary statistic you want to use; enter "mean", "max", or "min"
#'
#' @return dataframe of the data you want
#' @export
#'
#' @examples calc_swe_metrics(Chuska, "month", mean)
#' 

calc_swe_metrics = function(region, period, statistic){
  
  # dataframe of SWE metric desired
  av = region %>% 
    group_by(date = floor_date(date, period))  %>% 
    summarize(swe_mm = statistic(swe_mm), waterYear=min(waterYear)) 
  
  return(av)
  
}