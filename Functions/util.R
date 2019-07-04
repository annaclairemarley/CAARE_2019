
extract_winter_months = function (df) {
  filteredDF <- df %>% 
    rename(date = date_time) %>% 
    filter(month(date) %in% c(11, 12, 1, 2, 3, 4))
  return(filteredDF)
}

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
    dplyr:: rename(date = Date)
  
  return(wy_df)
}