#' calc_av_mn_swe 
#'
#' Creates dataframe for and graphs average monthly SWE
#'
#' @param region 
#'
#' @return
#' @export
#'
#' @examples

calc_av_mn_swe = function(region){
  
  # dataframe of annual maximum SWE
  av = region %>% 
    group_by(year_month = floor_date(date_time, "month"))  %>% 
    summarize(av_swe = mean(SWE_mm))
  
  # graph it:
  graph = av %>% 
    ggplot(aes(x = year_month, y = av_swe)) + 
    geom_line() +
    scale_y_continuous(expand = c(0,0)) +
    labs(
      x = "Year",
      y = "SWE (mm)"
    ) +
    theme_classic() 
  
  return(list(swe_mn_av = av, 
              swe_mn_av_graph = graph))
  
}