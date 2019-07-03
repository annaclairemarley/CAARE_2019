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
  
  startYear = year(min(df$wy_date))
  endYear = year(max(df$wy_date))
  labelDates = seq.Date(from = ymd(paste(year(min(df$wy_date)),11,1, sep='-')), length.out = endYear - startYear + 1, by = "1 year")
  breakDates = seq.Date(from = ymd(paste(year(min(df$wy_date)) - 1,11,1, sep='-')), length.out = endYear - startYear + 1, by = "1 year")
  
  swe_daily_graph = df %>% 
    ggplot(aes(x = Date, y = SWE_mm)) +
    geom_line() +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_date(breaks = unlist(breakDates, recursive = FALSE), labels=unlist(labelDates, recursive = FALSE)) +
    labs(
      x = "Year",
      y = "SWE (mm)",
      title = sprintf("%s", region)
    ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  return(swe_daily = swe_daily_graph)
 }