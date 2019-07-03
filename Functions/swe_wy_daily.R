
swe_wy_daily = function(df, region){
  
  startYear = year(min(df$water_date))
  endYear = year(max(df$water_date))
  labelDates = seq.Date(from = ymd(paste(year(min(df$water_date)),11,1, sep='-')), length.out = endYear - startYear + 1, by = "1 year")
  breakDates = seq.Date(from = ymd(paste(year(min(df$water_date)) - 1,11,1, sep='-')), length.out = endYear - startYear + 1, by = "1 year")
  
  swe_daily_graph = df %>% 
    ggplot(aes(x = Date, y = SWE_mm)) +
    geom_line() +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_date(breaks = unlist(breakDates, recursive = FALSE), labels=unlist(labelDates, recursive = FALSE)) +
    labs(
      x = "Water Year",
      y = "SWE (mm)",
      title = sprintf("%s", region)
    ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  return(swe_daily = swe_daily_graph)
  
  
}