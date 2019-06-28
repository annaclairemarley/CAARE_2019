#' calc_av_wk_swe
#' 
#' Creates dataframe for and plots average weekly SWE
#'
#' @param region 
#'
#' @return
#' @export
#'
#' @examples
#' 
calc_av_wk_swe = function(region){
  
  # dataframe 
  av = region %>% 
    group_by(year_week = floor_date(date_time, "1 week"))  %>% 
    summarize(av_swe = mean(SWE_mm))
  
  # graph it:
  graph = av %>% 
    ggplot(aes(x = year_week, y = av_swe)) + 
    geom_line() +
    scale_y_continuous(expand = c(0,0)) +
    labs(
      x = "Year",
      y = "SWE (mm)"
    ) +
    theme_classic() 
  
  return(list(swe_wk_av = av, 
              swe_wk_av_graph = graph))
  
}