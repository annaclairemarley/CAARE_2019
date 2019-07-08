
#####################################################################
# FUNCTIONS
#####################################################################

#' read_depth_chunks
#' 
#' function to read in date and depth data from separate files & join to one dataframe
#'
#' @param folder name of folder in your working directory, ie "Chuska" 
#'
#' @return dataframe
#' @export
#'
#' @examples read_depth_chunks("Carrizo")

read_depth_chunks = function(folder) {
  df = NULL
  for (file in list.files(folder, pattern = ".*_snodas_depth_.*")) {
    readFile = read_csv(paste(folder, file, sep="/"), col_names=c("date", "depth_mm"), skip = 1)
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
    filter(month(date) %in% c(month))
  
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