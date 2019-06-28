#' graph_swe_daily
#'
#' This function visualizes the raw daily SWE data from a region 
#'
#' @param region NN Mountain region
#'
#' @return graphs the daily SWE values
#' @export
#'
#' @examples

graph_swe_daily = function(region){
  
  swe_daily_graph = region %>% 
    ggplot(aes(x = date_time, y = SWE_mm)) +
    geom_line() +
    scale_y_continuous(expand = c(0,0)) +
    labs(
      x = "Year",
      y = "SWE (mm)"
    ) +
    theme_classic() 
  
  return(swe_daily = swe_daily_graph)
 }