#' graph_swe_metric
#' 
#' Graphs your swe metrics
#'
#' @param grouped_region the dataframe of the grouped summary statistic created w/ calc_swe_metrics()
#' @param type type of geom plot. examples are geom_line, geom_point, geom_col etc
#'
#' @return visualization of data
#' @export
#'
#' @examples

graph_swe_metric = function(grouped_region, type, region){
  graph = grouped_region %>% 
    ggplot(aes(x = date, y = swe_mm)) + 
    type() +
    scale_y_continuous(expand = c(0,0))+
    labs(
      x = "Year",
      y = "SWE (mm)",
      title = sprintf("%s", region)
    ) +
    theme_classic() 
  
  return(swe_graph = graph)
  
}