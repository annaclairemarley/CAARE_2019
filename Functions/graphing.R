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
    ggplot(aes(x = date, y = anomaly_perc)) +
    geom_col(aes(fill = sign), show.legend = FALSE) +
    #scale_y_continuous(limits = c(-20, 100)) +
    scale_fill_manual(values = c("negative" = "red", "positive" = "dark green")) +
    scale_x_date(date_breaks = "4 month", date_labels="%b", name="Month",
                 sec.axis = sec_axis(~. , name="Water Year",  breaks= breakDates, labels=seq(startWaterYear,endWaterYear,by=1))) +
    labs(
      x = "Date",
      y = "Anomaly (%)",
      title = sprintf("%s", title)
    ) +
    theme_classic() +
    theme(panel.border = element_rect(colour =
                                        "black", fill=NA, size=2),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.title.x = element_text(size = 12)) 
  
  
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
    scale_fill_manual(values=c("#D53E4F", "#FC8D59", "#FEE08B", "#E6F598", "#99D594", "#3288BD")) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(plot)
}

## same thing but for swe
month_bplot_swe = function(df, title = "") {
  
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
    ggplot(aes(x = fct_reorder(region, swe_mm, .desc = TRUE), y = swe_mm))+
    geom_boxplot(aes(fill = region_order), show.legend = FALSE) +
    labs(
      x = "Region",
      y = "SWE (mm)",
      title = sprintf("%s", title)
    ) + 
    scale_fill_manual(values=c("#D53E4F", "#FC8D59", "#FEE08B", "#E6F598", "#99D594", "#3288BD")) +
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
      scale_x_date(expand = c(0,0), date_breaks = "3 years", date_labels = "%Y") +
      scale_fill_manual(values = c("negative" = "#df5e3d", "positive" = "#a6c39d")) +
      geom_smooth(method = "lm", se = FALSE) +
      theme_classic()
  
    return(plot)
}

#####################################################################
#####################################################################
#' plot_pdsi_swe
#' 
#' plots swe anomaly and pdsi, use the output dataframe of combine_swe_pdsi()
#'
#' @param df dataframe of pdsi and swe
#' @param title the name of what the pdsi is for
#'
#' @return 

plot_pdsi_swe = function(df, title = ""){
  
  plot = df %>% 
    ggplot(aes(x = date)) + 
    geom_col(aes(y = anomaly_perc/100), fill = "#9dc6e0", show.legend = FALSE) +
    labs(
      x = "Year",
      y = "SWE Percent different from average (scaled down by 100)",
      title = sprintf("Chuska SWE and %s PDSI", title)
    ) +
    scale_x_date(expand = c(0,0)) +
    geom_line(aes(y = pdsi)) +
    scale_y_continuous(sec.axis = sec_axis(~., name = "PDSI"), expand = c(0,0)) +
    #scale_color_manual(values = c("negative" = "#df5e3d", "positive" = "#a6c39d")) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    theme_classic() +
    theme(axis.line.y.right = element_line(color = "#1219cc"),
          axis.text.y.right = element_text(color = "#1219cc"),
          axis.title.y.right = element_text(color = "#1219cc")) 

  return(plot)
}

#####################################################################
#####################################################################
#' plot_cor_swe_pdsi2
#' 
#' plots the correlation between swe and pdsi
#'
#' @param df df 
#' @param title whatever you want the title to be
#'
#' @return correlation between swe and pdsi, trendline and r value


plot_cor_swe_pdsi2 = function(df, title = ""){
  
  coef = cor.test(df$swe_anom, df$pdsi)$estimate
  p_value = cor.test(df$swe_anom, df$pdsi)$p.value
  
  # plot the correlation
  cor_plot_test <- df %>%
    ggplot(aes(x = swe_anom, y = pdsi)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)+
    labs(
      x = "SWE Anomaly",
      y = "PDSI",
      title = sprintf("%s", title),
      subtitle = sprintf("r = %s | p-value = %s", round(coef, 3), round(p_value, 3))
    ) +
    theme_classic()
  
  return(cor_plot_test)
}

#####################################################################
#####################################################################
#' plot_swe_pdsi
#' 
#' plots swe anomaly and pdsi
#'
#' @param df df 
#' @param month months of interest
#' @param region_time the name of the region and the month of spi your plotting
#'
#' @return bar graph of spi during the time you want it
#' @example plot_swe_spi(tsaile_spi, month = c(6,7,6), title = "Tsaile June")

plot_swe_pdsi = function(pdsi_df, month, swe_df = ch_wint_anom, region_time = "") {
  
  # clean up spi df and combine with chuska winter anomaly
  
 pdsi_clean <- pdsi_df %>% 
    filter(year(date) >= 2004) %>% 
    filter(month(date) %in% c(month)) %>% 
    add_water_year() %>% 
    select(waterYear, pdsi, sign) 
  
  # now combine winter chuska swe anom and the SPI
  ch_pdsi_plot <- pdsi_clean %>% 
    merge(swe_df, by = "waterYear") %>% 
    select(waterYear, anomaly_perc, pdsi) %>% 
    rename(PDSI = pdsi,
           "SWE Anomaly" = anomaly_perc) %>% 
    melt(id.vars = "waterYear") %>% 
    rename(metric = variable) %>% 
    ggplot(aes(x = waterYear, y = value, fill = metric)) +
    scale_fill_manual(values = c("#3288BD", "#99D594")) +
    geom_col(position = "dodge") +
    labs(
      x = "Water Year",
      y = "Value",
      fill = "Metric",
      title = sprintf("Winter SWE Anomaly and %s PDSI", region_time)
    )+
    theme_classic()
  
  return(ch_pdsi_plot)
}


#####################################################################
#####################################################################
#' plot_cor_swe_spi
#' 
#' plots the correlation between swe and spi
#'
#' @param df df 
#' @param title whatever you want the title to be
#'
#' @return correlation between swe and spi, trendline and r value


plot_cor_swe_spi = function(df, title = ""){
  
  coef = cor.test(df$swe_anom, df$spi)$estimate
  p_value = cor.test(df$swe_anom, df$spi)$p.value
  
  # plot the correlation
  cor_plot_test <- df %>%
    ggplot(aes(x = swe_anom, y = spi)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)+
    labs(
      x = "SWE Anomaly",
      y = "SPI",
      title = sprintf("%s", title),
      subtitle = sprintf("r = %s | p-value = %s", round(coef, 3), round(p_value, 3))
    ) +
    theme_classic()
  
  return(cor_plot_test)
}

#####################################################################
#####################################################################
#' plot_spi
#' 
#' plots spi
#'
#' @param df df 
#' @param month is the months you want to calculate spi for 
#' @param title whatever you want the title to be
#' @param years is whether you want the whole timeline or just from 2004 water year
#'
#' @return bar graph of spi during the time you want it
#' @example plot_spi(tsaile_spi, month = c(6,7,6), title = "Tsaile SPI")


plot_spi = function(df, month, title = "", years = "2004") {
    
  if (years == "all") {
  
    spi_clean <- df %>% 
      filter(month(date) %in% c(month)) 
    
  } else {
    
    spi_clean <- df %>% 
      filter(month(date) %in% c(month)) %>% 
      filter(year(date) >= 2004)
  }
  
  spi_graph <- spi_clean %>% 
      ggplot(aes(x = date, y = spi)) +
      geom_col(aes(fill = sign), show.legend = FALSE) +
      scale_fill_manual(values = c("negative" = "#df5e3d", "positive" = "#a6c39d")) +
      labs(
        y = "SPI",
        x = "Water Year",
        title = sprintf("%s", title)
      ) +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_date(expand = c(0,0), date_breaks = "3 years", date_labels = "%Y") +
      geom_smooth(method = "lm", se = FALSE) +
      theme_classic()

    return(spi_graph)
}

#####################################################################
#####################################################################
#' plot_swe_spi
#' 
#' plots swe anomaly and spi
#'
#' @param df df 
#' @param month months of interest
#' @param region_time the name of the region and the month of spi your plotting
#'
#' @return bar graph of spi during the time you want it
#' @example plot_swe_spi(tsaile_spi, month = c(6,7,6), title = "Tsaile June")

plot_swe_spi = function(spi_df, month, swe_df = ch_wint_anom, region_time = "") {

  # clean up spi df and combine with chuska winter anomaly
     
      spi_clean <- spi_df %>% 
      filter(year(date) >= 2004) %>% 
      filter(month(date) %in% c(month)) %>% 
      add_water_year() %>% 
      select(waterYear, spi, sign) 
    
  # now combine winter chuska swe anom and the SPI
  ch_spi_plot <- spi_clean %>% 
    merge(swe_df, by = "waterYear") %>% 
    select(waterYear,anomaly_perc, spi) %>% 
    rename(SPI = spi,
           "SWE Anomaly" = anomaly_perc) %>% 
    melt(id.vars = "waterYear") %>% 
    rename(metric = variable) %>% 
    ggplot(aes(x = waterYear, y = value, fill = metric)) +
    scale_fill_manual(values = c("#3288BD", "#99D594")) +
    geom_col(position = "dodge") +
    labs(
      x = "Water Year",
      y = "Value",
      fill = "Metric",
      title = sprintf("Winter SWE Anomaly and %s SPI", region_time)
    )+
    theme_classic()

  return(ch_spi_plot)
}

#####################################################################
#' plot_spi_drought_level
#' 
#' color codes the severity of the SPI drought for different months
#'
#' @param df df 
#' @param month months of interest
#' @param region_time the name of the region and the month of spi your plotting
#'
#' @return bar graph of spi during the time you want it
#' @example plot_swe_spi(tsaile_spi, month = c(6,7,6), title = "Tsaile June")

plot_spi_drought_level = function(spi_df, month, region_time = "", years = "") {
  
  # clean up spi df and combine with chuska winter anomaly
  
  if (years == "all") {
    
    spi_clean <- spi_df %>% 
      arrange(date) %>% 
      filter(month(date) %in% c(month)) %>% 
      add_water_year() %>% 
      select(waterYear, spi, sign) 
    
  } else {
    
    spi_clean <- spi_df %>% 
      arrange(date) %>% 
      filter(month(date) %in% c(month)) %>% 
      filter(year(date) >= 2004) %>% 
      add_water_year() %>% 
      select(waterYear, spi, sign) 
  }
       
  spi_clean$drought = "normal"
  for (i in length(spi_clean$spi)){
    spi_clean$drought = ifelse(spi_clean$spi <= -1.5,
                               "drought_emergency",
                               ifelse(spi_clean$spi <= -1.0,
                                      "drought_warning",
                                      ifelse(spi_clean$spi <= 0,
                                             "drought_alert",
                                             "normal")))
    }
           
  # now combine winter chuska swe anom and the SPI
  ch_spi_plot <- spi_clean %>% 
    rename(SPI = spi) %>% 
    ggplot(aes(x = waterYear, y = SPI, 
               fill = factor(drought, 
                             levels = c("drought_emergency",
                                        "drought_warning",
                                        "drought_alert",
                                        "normal"))
               ), color = "white") +
     scale_fill_manual(values = c("#d13111", 
                                  "#ffa600", 
                                  "#f1d333", 
                                  "#16990c"), 
                       labels = c("Drought Emergency", 
                                  "Drought Warning", 
                                  "Drought Alert", 
                                  "Normal")) +
    geom_col(position = "dodge2") +
    labs(
      x = "Water Year",
      y = "SPI",
      fill = "Drought Severity",
      title = sprintf("%s SPI Drought Conditions", region_time)
    )+
    theme_classic() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.title.x = element_text(size = 12))
  
  if (years == "all") {
    
    ch_spi_plot <- ch_spi_plot +
      scale_x_continuous(breaks = seq(1981,2019, by = 5))
    
  } else {
    
    ch_spi_plot <- ch_spi_plot +
      scale_x_continuous(breaks = seq(2004,2019, by = 2)) 
  }
  
  
  
  return(ch_spi_plot)
}

#####################################################################
#####################################################################
#' plot_cor_pdsi_spi
#' 
#' plots the correlation between pdsi and spi
#'
#' @param df df 
#' @param title whatever you want the title to be
#'
#' @return correlation between pdsi and spi, trendline and r value

# find and plot the correlation between the two

plot_cor_pdsi_spi = function(df, title = "") {

    coef <- cor.test(df$spi, df$mean_pdsi)$estimate
    p_value <- cor.test(df$spi, df$mean_pdsi)$p.value
    
    spi_pdsi_plot <- df %>%
      ggplot(aes(x = mean_pdsi, y = spi)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE)+
      labs(
        x = "PDSI",
        y = "SPI",
        title = sprintf("%s PDSI and SPI", title),
        subtitle = sprintf("r = %s | p-value: %s", round(coef,3), p_value) 
      ) +
      theme_classic()

    return(spi_pdsi_plot)
    
}

#####################################################################
#####################################################################
#' plot_spi_swe_all
#' 
#' plots April - July correlations of spi and swe
#'
#' @param spi_df the monthly spi data
#' @param swe_df whatever swe anomaly you want to use
#'



plot_spi_swe_all = function(spi_df, swe_df){
    
    spi_mar <- merge_ch_spi(spi_df, df_swe = swe_df, months = c(3)) 
    
    mar_plot <- plot_cor_swe_spi(spi_mar,
                                 title = "Winter SWE Anomaly & March SPI")
  
    spi_apr <- merge_ch_spi(spi_df, df_swe = swe_df, months = c(4)) 
    
    apr_plot <- plot_cor_swe_spi(spi_apr,
                                            title = "Winter SWE Anomaly & April SPI")
    
    spi_may <- merge_ch_spi(spi_df, df_swe = swe_df, months = c(5)) 
    may_plot <- plot_cor_swe_spi(spi_may,
                                            title = "Winter SWE Anomaly & May SPI")
    
    spi_june <- merge_ch_spi(spi_df, df_swe = swe_df, months = c(6)) 
    june_plot <- plot_cor_swe_spi(spi_june,
                                             title = "Winter SWE Anomaly & June SPI")
    
    spi_july <- merge_ch_spi(spi_df, df_swe = swe_df, months = c(7)) 
    july_plot <- plot_cor_swe_spi(spi_july,
                                             title = "Winter SWE Anomaly & July SPI")
    
    
    grid <- grid.arrange(mar_plot, apr_plot, may_plot, 
                 june_plot, ncol = 2)

    return(grid)
}

#####################################################################
#####################################################################
#' plot_spi_swe_box
#' 
#' plots chuska swe anomaly grouped by following spi drought severity
#'
#' @param chuska_spi_df df that has combination of month spi and chuska swe
#' @param month_name name of the month of the spi youre looking at
#'

plot_spi_swe_box = function(chuska_spi_df, month_name = ""){
 
   boxplot <- chuska_spi_df %>% 
    ggplot(aes(x = factor(drought, 
                          levels = c("emergency",
                                     "warning",
                                     "alert",
                                     "normal")), 
               y = anomaly_perc)) +
    geom_jitter(aes(color = factor(drought, 
                                   levels = c("emergency",
                                              "warning",
                                              "alert",
                                              "normal"))
    ), show.legend = FALSE) +
    geom_boxplot(aes(color = factor(drought, 
                                    levels = c("emergency",
                                               "warning",
                                               "alert",
                                               "normal")),
                     alpha = 0.001), show.legend = FALSE)+
    
    scale_color_manual(values = c("#d13111", 
                                  "#ffa600", 
                                  "#f1d333", 
                                  "#16990c")) +
    labs(
      x = sprintf("%s Drought Severity", month_name),
      y = "Chuska Winter SWE Anomaly",
      color = "Drought"
    ) +
    theme_classic()

    return(boxplot)
}

#####################################################################
#####################################################################
#' plot_spi_swe_point
#' 
#' plots chuska swe anomaly grouped by following spi drought severity
#'
#' @param chuska_spi_df df that has combination of month spi and chuska swe
#' @param month_name name of the month of the spi youre looking at
#'

plot_spi_swe_point = function(chuska_spi_df, month_name = "") {
 
   drought_times_graph <- chuska_spi_df %>% 
    ggplot(aes(x = anomaly_perc, y = spi)) +
    geom_point(aes(color = factor(drought, 
                                  levels = c("emergency",
                                             "warning",
                                             "alert",
                                             "normal")))) +
    scale_color_manual(values = c("#d13111", 
                                  "#ffa600", 
                                  "#f1d333", 
                                  "#16990c")) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      x = "Chuska SWE Winter Anomaly",
      y = sprintf("%s SPI", month_name),
      color = "Drought Severity"
    ) +
    theme_classic() 

  return(drought_times_graph)

}