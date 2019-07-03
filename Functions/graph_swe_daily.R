#' graph_swe_daily
#'
#' This function visualizes the raw daily SWE data from a region 
#'
#' @param 
#' @param region NN Mountain region
#'
#' @return graphs the daily SWE values
#' @export
#'
#' @examples

graph_swe_daily = function(df, region){
 
  swe_daily_graph = df %>% 
    ggplot(aes(x = date, y = SWE_mm)) +
    geom_line() +
    scale_y_continuous(expand = c(0,0)) +
    labs(
      x = "Year",
      y = "SWE (mm)",
      title = sprintf("%s", region)
    ) +
    theme_classic() 
  
  return(swe_daily = swe_daily_graph)
 }