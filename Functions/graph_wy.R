

graph_wy = function(df, type, region){
    graph = df %>% 
      ggplot(aes(x = date, y = swe_mm)) + 
      type() +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_date(date_labels = "%b%Y") +
      labs(
        x = "Year",
        y = "SWE (mm)",
        title = sprintf("%s", region)
      ) +
      theme_classic() 
    
    return(swe_graph = graph)
    
  
  
}