#' calc_max_mn_swe
#'
#' Creates a dataframe for and plots monthly max swe
#'
#' @param region 
#'
#' @return
#' @export
#'
#' @examples
#' 
calc_max_mn_swe = function(region){
  
  # dataframe 
  max = region %>% 
    group_by(year_month = floor_date(date_time, "month"))  %>% 
    summarize(max_swe = max(SWE_mm))
  
  # graph it:
  max_graph = max %>% 
    ggplot(aes(x = year_month, y = max_swe)) + 
    geom_line() +
    scale_y_continuous(expand = c(0,0)) +
    labs(
      x = "Year",
      y = "SWE (mm)"
    ) +
    theme_classic() 
  
  return(list(swe_mn_max = max, 
              swe_mn_max_graph = max_graph))
  
}