#' calc_max_yr_swe
#' 
#' Plots and makes a dataframe for yearly max swe
#'
#' @param region 
#'
#' @return
#' @export
#'
#' @examples

calc_max_yr_swe = function(region){

# dataframe of annual maximum SWE
yr_max = region %>% 
  group_by(year = floor_date(date_time, "year"))  %>% 
  summarize(max_swe = max(SWE_mm))

# graph it:
yr_max_graph = yr_max %>% 
  ggplot(aes(x = year, y = max_swe)) + 
  geom_line() +
  scale_y_continuous(expand = c(0,0)) +
  labs(
    x = "Year",
    y = "SWE (mm)"
  ) +
  theme_classic() 

return(list(swe_yr_max = yr_max, 
            plt = yr_max_graph))

}