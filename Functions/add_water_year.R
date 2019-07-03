#' add_water_year
#' 
#' Changes the date column to water year & changes months as November - April (so 1= nov, 6 = april)
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
    dplyr::mutate(month = month(Date),
           day = day(Date)) 
  
   # for (i  in 1:length(wy_df$month)) {
    #  wy_df$month[i] = ifelse(wy_df$month[i] == "4", paste("06"), wy_df$month[i])
     # wy_df$month[i] = ifelse(wy_df$month[i] == "3", paste("05"), wy_df$month[i])
    #  wy_df$month[i] = ifelse(wy_df$month[i] == "2", paste("04"), wy_df$month[i])
    #  wy_df$month[i] = ifelse(wy_df$month[i] == "1", paste("03"), wy_df$month[i])
    #  wy_df$month[i] = ifelse(wy_df$month[i] == "11", paste("01"), wy_df$month[i])
    #  wy_df$month[i] = ifelse(wy_df$month[i] == "12", paste("02"), wy_df$month[i])
    #}
    
    wy_df = wy_df %>% 
    mutate(water_date = (paste(waterYear, month, day, sep="-"))) %>% 
    dplyr::select(Date, water_date, waterYear, SWE_mm) 
  
  return(water_year_df = wy_df)
}