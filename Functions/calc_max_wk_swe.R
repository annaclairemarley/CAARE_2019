#' calc_max_wk_swe
#' 
#' Creates dataframe for and graphs weekly maximum swe
#'
#' @param region 
#'
#' @return
#' @export
#'
#' @examples

calc_max_wk_swe = function(region){
  
  # dataframe of annual maximum SWE
  max = region %>% 
    group_by(year_week = floor_date(date_time, "1 week"))  %>% 
    summarize(max_swe = max(SWE_mm))
  
  # graph it:
  max_graph = max %>% 
    ggplot(aes(x = year_week, y = max_swe)) + 
    geom_line() +
    scale_y_continuous(expand = c(0,0)) +
    labs(
      x = "Year",
      y = "SWE (mm)"
    ) +
    theme_classic() 
  
  return(list(swe_wk_max = max, 
              swe_wk_max_graph = max_graph))
  
}