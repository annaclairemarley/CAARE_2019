
#####################################################################
# FUNCTIONS
#####################################################################

#' read_chunks
#' 
#' function to read in date and data from separate files & join to one dataframe
#'
#' @param folder name of folder in your working directory, ie "Chuska" 
#'
#' @return dataframe
#' @export
#'
#' @examples read_depth_chunks("Carrizo", fil_pattern = ".*_snodas_depth_.*", variable = "depth_mm")

read_chunks = function(folder, fill_pattern = ".*_snodas_depth_.*", variable = "depth_mm") {
  df = NULL
  for (file in list.files(folder, pattern = fill_pattern)) {
    readFile = read_csv(paste(folder, file, sep="/"), col_names=c("date", variable), skip = 1)
    if (is.null(df)) { # don't merge the first one
      df = readFile
    } else {
      df = rbind(df, readFile) # bind the files together
    }
  }
  return(df)
}

#' extract_winter_months
#' 
#' CExtracts winter months and standardises column names
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples

extract_winter_months = function (df) {
  filteredDF <- df %>% 
    rename(date = date_time, swe_mm = SWE_mm) %>% 
    filter(month(date) %in% c(11, 12, 1, 2, 3, 4))
  return(filteredDF)
}

#####################################################################
#####################################################################

#' add_water_year
#' 
#' Changes the date column to water year
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples

add_water_year = function(df) {
  
  wy_df = df %>% 
    dplyr:: rename(Date = date) %>%
    dataRetrieval::addWaterYear() %>%
    dplyr:: rename(date = Date)
  
  return(wy_df)
}

#####################################################################
#####################################################################

# just to make merging snow depth and snow water equivalent easier

merge_snow = function(df1, df2) {
  
 new_df =  merge(df1, df2, by = "date") %>% 
    select(-waterYear.y) %>% 
    rename(waterYear = waterYear.x) %>% 
    mutate(month = month(date)) %>% 
    mutate(snow_ratio = swe_mm/depth_mm)
 
  return(new_df)
}

#####################################################################
#####################################################################



#####################################################################
#####################################################################

#' calc_swe_metrics
#'
#' Makes a dataframe of the summary statistic over the time period you want
#'
#' @param region 
#' @param period the time period you want to group by; enter "month", "year", or "1 week" 
#'       **make sure you put the period you want in quotes!!!**
#' @param statistic the summary statistic you want to use; enter "mean", "max", or "min"
#'
#' @return dataframe of the data you want
#' @export
#'
#' @examples calc_swe_metrics(Chuska, "month", mean)
#' 

calc_swe_metrics = function(region, period, statistic){
  
  # dataframe of SWE metric desired
  av = region %>% 
    group_by(date = floor_date(date, period))  %>% 
    summarize(swe_mm = statistic(swe_mm), waterYear=min(waterYear)) 
  
  return(av)
  
}

# same thing but for depth
calc_depth_metrics = function(region, period, statistic){
  
  # dataframe of SWE metric desired
  av = region %>% 
    group_by(date = floor_date(date, period))  %>% 
    summarize(depth_mm = statistic(depth_mm), waterYear=min(waterYear)) 
  
  return(av)
  
}

# same thing but for pdsi
calc_pdsi_metrics = function(region, period, statistic){
  
  # dataframe of SWE metric desired
  av = region %>% 
    group_by(date = floor_date(date, period))  %>% 
    summarize(pdsi = statistic(pdsi), waterYear=min(waterYear)) 
  
  return(av)
  
}

#####################################################################
#####################################################################

#' calc_month_anom
#'
#' @param df dataframe of monthly means or max
#'
#' @return dataframe that has anomaly averaged from each month across years
#' @export
#'
#' @examples

calc_month_anom = function(df){
  
  df_month = df %>%
    mutate(month = month(date))  
  
  intra_month = df_month %>% 
    group_by(month) %>% 
    summarize(mean_swe = mean(swe_mm))
  
  intra_month_anomaly = merge(df_month, intra_month, by = "month")%>%
    mutate(anomaly = swe_mm - mean_swe) %>% 
    mutate(anomaly_perc = (anomaly/mean_swe)*100) %>% 
    mutate(sign = ifelse(anomaly < 0, "negative", "positive") )
  
  return(intra_month_anomaly = intra_month_anomaly)
}

## same but for depth
calc_mnth_depth_anom = function(df){
  
  df_month = df %>%
    mutate(month = month(date))  
  
  intra_month = df_month %>% 
    group_by(month) %>% 
    summarize(mean_depth = mean(depth_mm))
  
  intra_month_anomaly = merge(df_month, intra_month, by = "month")%>%
    mutate(anomaly = depth_mm - mean_depth) %>% 
    mutate(anomaly_perc = (anomaly/mean_depth)*100) %>%
    mutate(sign = ifelse(anomaly < 0, "negative", "positive") )
  
  return(intra_month_anomaly = intra_month_anomaly)
}

#####################################################################
#####################################################################

#' month_ts
#'
#' @param df dataframe of monthly means or max
#' @param month month you want to create the dataframe for
#'
#' @return dataframe that has anomaly averaged from each month across years
#' @export
#'
#' @examples

month_ts = function(df, month) {
  df = df %>% 
    dplyr::filter(month(date) %in% c(month))
  
  return(df) 
}


#####################################################################
#####################################################################

#' join_month
#'
#' @param df combines dataframes of the single months for each region into one df
#'  need to input name you want the df to be called and the df
#' 
#' @return dataframe that has anomaly averaged from each month across years
#' @export
#'
#' @examples join_months(carrizo = car_nov, chuska = ch_nov)


join_months = function(...) {
  kwargs = list(...) # key word arguents 
  placeNames = names(kwargs) # grabs the name of what you input
  
  mergerdDF = NULL
  for (i in 1:length(placeNames)) { # for each placename
    df = kwargs[[placeNames[i]]] %>% # gets the df for that placename
      select(waterYear, swe_mm) %>% #select water year and swe_mm
      rename(!!placeNames[i] := swe_mm) #renames swe_mm to the placename for joining
    if (i == 1){ # if it's the 1st df, don't need to merge
      mergedDF = df
    } else{
      mergedDF = merge(mergedDF, df, by="waterYear") #all the subsequent ones you merge together
    }
  }
  
  result = mergedDF %>% 
    melt(id.vars="waterYear") %>% # then you just melt and rename
    rename(region = variable,
           swe_mm = value)
  return(result)
  
}



## same but for depth
join_depth_months = function(...) {
  kwargs = list(...) # key word arguents 
  placeNames = names(kwargs) # grabs the name of what you input
  
  mergerdDF = NULL
  for (i in 1:length(placeNames)) { # for each placename
    df = kwargs[[placeNames[i]]] %>% # gets the df for that placename
      select(waterYear, depth_mm) %>% #select water year and swe_mm
      rename(!!placeNames[i] := depth_mm) #renames swe_mm to the placename for joining
    if (i == 1){ # if it's the 1st df, don't need to merge
      mergedDF = df
    } else{
      mergedDF = merge(mergedDF, df, by="waterYear") #all the subsequent ones you merge together
    }
  }
  
  result = mergedDF %>% 
    melt(id.vars="waterYear") %>% # then you just melt and rename
    rename(region = variable,
           depth_mm = value)
  return(result)
  
}

# same but for swe_km3
join_months_m = function(...) {
  kwargs = list(...) # key word arguents 
  placeNames = names(kwargs) # grabs the name of what you input
  
  mergerdDF = NULL
  for (i in 1:length(placeNames)) { # for each placename
    df = kwargs[[placeNames[i]]] %>% # gets the df for that placename
      select(waterYear, swe_total_m3) %>% #select water year and swe_mm
      rename(!!placeNames[i] := swe_total_m3) #renames swe_mm to the placename for joining
    if (i == 1){ # if it's the 1st df, don't need to merge
      mergedDF = df
    } else{
      mergedDF = merge(mergedDF, df, by="waterYear") #all the subsequent ones you merge together
    }
  }
  
  result = mergedDF %>% 
    melt(id.vars="waterYear") %>% # then you just melt and rename
    rename(region = variable,
           swe_total_m3 = value)
  return(result)
  
}

#####################################################################
#####################################################################


cor_equation = function(df) {
  
  # Months, Intercept, Slope
  result_df = NULL
  
  for (month_no in unique(df$month)) {
    
    df_month = df %>% 
      filter(month == month_no)
    
    lm_df = lm(df_month$swe_mm ~ df_month$depth_mm)

    intercept = lm_df$coefficients[1]
    slope = lm_df$coefficients[2]
    r.squared = summary(lm_df)$adj.r.squared
    
    if (is.null(result_df)) {
      result_df = data.frame(month=c(month_no),
                      intercept=c(intercept),
                      slope=c(slope),
                      r.squared = c(r.squared))
    } else {
      result_df = rbind(result_df, c(month_no, intercept, slope, r.squared))
    }
     
    
  }

  result_df = result_df %>% 
    mutate(month_n = c("November", "December", "January", "February", "March", "April")) %>% 
    mutate(month_n = factor(month_n, levels = c("November", "December", "January", "February", "March", "April")))
    
  
  return(result_df)
}

#####################################################################
#####################################################################

#' get_year_max
#'
#' create a dataframe with maximum swe_mm weekly average per year
#'
#' @param df dataframe of average weekly values
#'
#' @return dataframe with maximum swe_mm weekly average per year
#' @export
#'
#' @examples get_year_max(ch_weekly)


get_year_max  = function(df) {
  max_results = data.frame(date=as.Date(character()), 
                           swe_mm=integer(),
                           waterYear=integer()) 
  for (year in unique(df$waterYear)) {
    yearRecords = filter(df, waterYear == year)
    max_record = yearRecords[which.max(yearRecords$swe_mm),]
    max_results = rbind(max_results, max_record)
  }
  return(max_results)
}

#####################################################################
#####################################################################

#' to_water_week
#'
#' Gets the water week number for a given date (week starts on Sunday)
#'
#' @param date the date to get the water week number for. N.B. this has to be a Date object, not a string
#'
#' @return water week number for a given date (1-52)
#' @export
#'
#' @examples to_water_week(date)
#' @examples mutate(week_water_year = to_water_week(date))


to_water_week = function(date) {
  octStartWeek = epiweek(ymd(paste(year(date), "-10-01")))
  diff = 52 - octStartWeek
  return((epiweek(date) + diff) %% 52 + 1)
  
}