#' graph_swe_metric
#' 
#' Graphs your swe metrics
#'
#' @param df the dataframe to display. Expects daata to display to be in column swe_mm and date axis to be named date. Should also contain column called waterYear
#' @param title Title of plot
#' @param type type of geom plot. examples are geom_line, geom_point, geom_col etc
#'
#' @return visualization of data
#' @export
#'
#' @examples
#' 
graph_with_wateryear = function(df, title = "", type=geom_line, variable = "swe_mm", ylab = "SWE (mm)") {
  
  # calculate start and end water year for graphing
  startWaterYear = min(df$waterYear)
  endWaterYear = max(df$waterYear)
  
  # Create sequence of real dates, on which to place the water year lables
  breakDates = seq.Date(from = ymd(paste(startWaterYear,1,1, sep='-')), length.out = endWaterYear - startWaterYear + 1, by = "1 year")
  
  swe_daily_graph = df %>% 
    ggplot(aes_string("date", variable)) +
    type(color = "dark blue") +
    scale_x_date(date_breaks = "3 month", date_labels="%b", name="Month",
                 sec.axis = sec_axis(~. , name="Water Year",  breaks= breakDates, labels=seq(startWaterYear,endWaterYear,by=1))
    ) +
    scale_y_continuous(expand = c(0,0)) +
    labs(
      x = "Month",
      y = ylab,
      title = sprintf("%s", title)
    ) +
    theme_classic()
  
  return(swe_daily = swe_daily_graph)
  
}

#####################################################################
#####################################################################

#' plot_anomaly
#' 
#' Plots the anomaly of the dataframe. Best if input the monthly mean dataframe into this as it
#' will calculate the mean of all the monthly means and then make an anomaly
#'
#' @param df monthly mean dataframe
#' @param title put the region in quotes like title = "Carrizo" to label the graph
#'
#' @return Anomaly graph 
#' @export
#'
#' @examples

plot_anomaly = function(df, title = ""){
  
  startWaterYear = min(df$waterYear)
  endWaterYear = max(df$waterYear)
  
  # Create sequence of real dates, on which to place the water year lables
  breakDates = seq.Date(from = ymd(paste(startWaterYear,1,1, sep='-')), length.out = endWaterYear - startWaterYear + 1, by = "1 year")
  
  plot = df %>% 
    ggplot(aes(x = date, y = anomaly)) +
    geom_col(aes(fill = sign), show.legend = FALSE) +
    #scale_y_continuous(limits = c(-20, 100)) +
    scale_fill_manual(values = c("negative" = "red", "positive" = "dark green")) +
    scale_x_date(date_breaks = "3 month", date_labels="%b", name="Month",
                 sec.axis = sec_axis(~. , name="Water Year",  breaks= breakDates, labels=seq(startWaterYear,endWaterYear,by=1))) +
    labs(
      x = "Date",
      y = "Anomaly",
      title = sprintf("%s", title)
    ) +
    theme_classic()
  
  
  return(anomaly_plot = plot)
}

## if you just want to plot monthly anomalies
plot_monthly_anomaly = function(df, month_no, title = ""){

  df = df %>% 
    filter(month == month_no)
  
plot = df %>% 
  ggplot(aes(x = waterYear, y = anomaly)) +
  geom_col(aes(fill = sign), show.legend = FALSE) +
  scale_fill_manual(values = c("negative" = "red", "positive" = "dark green")) +
  labs(
    x = "Water Year",
    y = "Anomaly",
    title = sprintf("%s", title)
  ) +
  theme_classic()

return(plot)

}
#####################################################################
#####################################################################

#' plot_swe_month
#' 
#' Plots the monthly metric of each region, 
#'
#' @param df monthly metric dataframe of each region already narrowed down to month you want using month_ts
#'
#' @return  
#' @export
#'
#' @examples plot_swe_month(carr_monthly, ch_monthly, bm_monthly)

plot_months = function(df, month = "") {

  plot = df %>% 
  ggplot(aes(x = waterYear, y = swe_mm)) +
  geom_col(aes(fill = region)) +
  scale_x_continuous(breaks=seq(2004,2019, by = 2)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Water Year",
       y = "SWE (mm)",
       title = sprintf("SWE %s Month``", month)
       ) +
  theme_classic()

  return(plot)

}

#####################################################################
#####################################################################

#' graph_anwy_metrics
#'
#' For annual water year metrics
#'
#' @param region water year dataframe
#' @param statistic mean, median, max etc
#'
#' @return
#' @export
#'
#' @examples

graph_anwy_metrics = function(region, statistic, title = ""){
  
  # dataframe of SWE metric desired
  av = region %>% 
    group_by(waterYear)  %>% 
    summarize(depth_mm = statistic(depth_mm)) 
  
  graph = av %>% 
    ggplot(aes(x = waterYear, y = depth_mm )) +
    geom_col() +
    labs(
      x = "Water Year",
      y = "Snow Depth (mm)",
      title = sprintf("%s", title)
    ) +
    scale_y_continuous(expand = c(0,0)) +
    theme_classic()
  
  return(graph)
  
}

#####################################################################
#####################################################################
#' plot_cor_month
#' 
#' Plots the correlation between snow depth and swe
#'
#' @param df needs to be combined df of swe and snow depth 
#' @param adjr2 the name of the adjusted r2 value of the swe and depth correlation of that region
#'
#' @return correlation graph
#' @export
#'
#' @examples plot_cor(df, car_r2, region = "Carrizo)

plot_cor_month = function(df, adjr2, region = "") {
  
  graph = df %>% 
    ggplot(aes(x = depth_mm, y = swe_mm, color = as.factor(month))) +
   # geom_point() +
    labs(x = "Snow Depth (mm)",
         y = "SWE (mm)",
         fill = "Month",
         color = "Month",
         title = sprintf("%s", region)) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_classic()
  return(graph)
}

#####################################################################
#####################################################################
#' plot_cor
#' 
#' Plots the correlation between snow depth and swe
#'
#' @param df needs to be combined df of swe and snow depth 
#' @param adjr2 the name of the adjusted r2 value of the swe and depth correlation of that region
#'
#' @return correlation graph
#' @export
#'
#' @examples plot_cor(df, car_r2, region = "Carrizo)

plot_cor = function(df, adjr2, region = "") {
  
  graph = df %>% 
    ggplot(aes(x = depth_mm, y = swe_mm)) +
     geom_point(aes(color = as.factor(month))) +
    labs(x = "Snow Depth (mm)",
         y = "SWE (mm)",
         fill = "Month",
         color = "Month",
         title = sprintf("%s", region),
         subtitle = paste("Adj R2 = ", round(adjr2, 3))) +
    geom_smooth(method = "lm", color = "black") +
    theme_classic()
  
}

#####################################################################
#####################################################################

# to make the averaged month over region boxplots



month_bplot_dpth = function(df, title = "") {

  # make a loop to set the order
  df$region_order = 0
  for (i in length(df$region)) {
    df$region_order = ifelse(df$region == "chuska", "F", df$region_order)
    df$region_order = ifelse(df$region == "defiance_plateau", "E", df$region_order)
    df$region_order = ifelse(df$region == "black_mesa", "D", df$region_order)
    df$region_order = ifelse(df$region == "carrizo", "C", df$region_order)
    df$region_order = ifelse(df$region == "mt_powell", "B", df$region_order)
    df$region_order = ifelse(df$region == "navajo_mt", "A", df$region_order)
  }
  
  plot = df %>% 
    ggplot(aes(x = fct_reorder(region, depth_mm, .desc = TRUE), y = depth_mm))+
    geom_boxplot(aes(fill = region_order), show.legend = FALSE) +
    labs(
      x = "Region",
      y = "Depth (mm)",
      title = sprintf("%s", title)
    ) + 
    #scale_y_continuous(limits = c(0, 950)) +
    #scale_fill_brewer(palette = "Spectral", direction = -1)+
    scale_fill_manual(values=c("#D53E4F", "#FC8D59", "#FEE08B", "#E6F598", "#99D594", "#3288BD")) +
   #scale_x_discrete(labels = c("Chuska" = "chuska", "Mt. Powell"= "mt_powell", "Black Mesa" = "black_mesa", "Defiance Plateau" = "defiance_plateau", "Carrizo" = "carrizo", "Navajo Mt" = "navajo_mt")) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(plot)
}



#####################################################################
#####################################################################
#' plot_slope_swedpth
#' 
#' Plots the slope of the line of the linear relationship between swe and snowdepth over the months
#'
#' @param df needs to be the output dataframe of cor_equation()
#' @param title name of the region
#'
#' @return correlation graph
#' @export
#'
#' @examples plot_slope_swedpth(df, title = "Carrizo")

plot_slope_swedpth = function(df, title = "") {

  graph = df %>% 
    ggplot(aes(x = month_n, y = slope)) +
    geom_point() +
    labs(
      x = "Month",
      y = "Slope",
      title = sprintf("%s", title)
    ) +
    theme_classic()

  return(graph)
}

#####################################################################
#####################################################################
#' plot_wk_max_time
#' 
#' Plots what week of the year the maximum swe_mm weekly average per year occurs for each water year
#'
#' @param df needs to be the output dataframe of:
#'              df %>% get_year_max() %>% mutate(wk_of_wy = to_water_week(date))
#' @param title name of the region
#'
#' @return graph with trendline
#' @export
#'
#' @examples pplot_wk_max_time(ch_max_weekly_yr, title = "Chuska")

plot_wk_max_time = function(df, title = "") {
  
  # fit a linear trendline and get the coefficients for the graph
  lm = lm(df$wk_of_wy ~ df$waterYear)
  intercept = coef(lm)[1]
  slope = coef(lm)[2]
  pvalue = summary(lm)$coefficients[2,4]
  
  # graph it
  graph = df %>% 
    ggplot(aes(x = waterYear, y = wk_of_wy)) + 
    geom_line() +
    labs(
      x = "Water Year",
      y = "Week of Water Year",
      title = sprintf("%s", title),
      subtitle = sprintf("Trendline: y = %sx + %s \n Slope p-value: %s", 
                         round(slope, 3), round(intercept,3), round(pvalue,2))
    ) +
    scale_x_continuous(expand = c(0,0), breaks = seq(2004,2019, by = 2)) +
    ylim(5,30)+
    geom_smooth(method = "lm", se = FALSE) +
    theme_classic()

  return(graph)
}

#####################################################################
#####################################################################
#' plot_cor_swe_pdsi
#' 
#' plots the correlation between swe and pdsi
#'
#' @param df df created with compare_swe_pdsi()
#' @param title whatever you want the title to be
#'
#' @return correlation between swe and pdsi, trendline and r value


plot_cor_swe_pdsi = function(df, title = ""){
  
  coef = cor.test(df$mean_swe, df$mean_pdsi)$estimate
  
  # plot the correlation
  cor_plot_test <- df %>%
    ggplot(aes(x = mean_swe, y = mean_pdsi)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)+
    labs(
      x = "SWE Anomaly",
      y = "PDSI",
      title = sprintf("%s", title),
      subtitle = sprintf("r = %s", round(coef, 3))
    ) +
  theme_classic()

  return(cor_plot_test)
}

#####################################################################
#####################################################################
#' plot_pdsi
#' 
#' plots basic pdsi trend
#'
#' @param df dataframe of pdsi
#' @param title the name of what the pdsi is for
#'
#' @return 

plot_pdsi = function(df, title = ""){
  
    plot = df %>% 
      ggplot(aes(x = date, y = pdsi)) + 
      geom_col(aes(fill = sign), show.legend = FALSE) +
      labs(
        x = "Year",
        y = "PDSI",
        title = sprintf("%s Palmer Drought Severity Index (PDSI)", title)
      ) +
      scale_x_date(expand = c(0,0)) +
      scale_fill_manual(values = c("negative" = "#df5e3d", "positive" = "#a6c39d")) +
      geom_smooth(method = "lm") +
      theme_classic()
  
    return(plot)
}