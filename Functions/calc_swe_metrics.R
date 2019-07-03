#' calc_swe_metrics
#'
#' Makes a dataframe of the summary statistic over the time period you want
#'
#' @param region 
#' @param period the time period you want to group by; enter "month", "year", or "1 week" 
#'       **make sure you put the period you want in quotes!!!**
#' @param statistic the summary statistic you want to use; enter "mean", "max", or "min"
#' 
#'
#' @return dataframe of the data you want
#' @export
#'
#' @examples calc_swe_metrics(Chuska, "month", mean)
#' 

calc_swe_metrics = function(region, period, statistic){
  
  # dataframe of SWE metric desired
  av = region %>% 
    group_by(date = floor_date(Date, period))  %>% 
    summarize(swe_mm = statistic(SWE_mm)) 

  return(swe_mn_av = av)
  
}