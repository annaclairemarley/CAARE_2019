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
  numberWaterYears =  length(unique(df$waterYear))
  swe_daily_graph = df %>% 
    ggplot(aes(x = date, y = SWE_mm)) +
    geom_line() +
    scale_x_date(date_breaks = "3 month", date_labels="%b", sec.axis=
                   sec_axis(~.  ,breaks= seq(2004,2019,by=2), labels=seq(2004,2019,by=2))) +
    scale_y_continuous(expand = c(0,0)) +
#    annotate(geom = "text", x = 2.5 + 4 * (0:(numberWaterYears - 1)), y = 32, label = unique(df$waterYear), size = 20) +
    labs(
      x = "Year",
      y = "SWE (mm)",
      title = sprintf("%s", region)
    ) +
    theme_classic()
  
  return(swe_daily = swe_daily_graph)
 }