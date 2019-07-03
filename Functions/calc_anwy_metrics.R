#' calc_anwy_metrics
#'
#' For annual water year metrics
#'
#' @param region water year dataframe
#' @param statistic mean, median, max etc
#'
#' @return
#' @export
#'
#' @examples

calc_anwy_metrics = function(region, statistic){
  
  # dataframe of SWE metric desired
  av = region %>% 
    group_by(waterYear)  %>% 
    summarize(swe_mm = statistic(SWE_mm)) 
  
  return(swe_mn_av = av)
  
}