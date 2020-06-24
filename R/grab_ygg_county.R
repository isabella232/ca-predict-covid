grab_ygg_county <- function(State = "California"){
  
  
  url <-paste0("https://github.com/youyanggu/covid19_projections/raw/master/projections/combined/latest_subregion.csv")
  
  if (as.character(url_file_exists(url)[1]) == "TRUE" ) {
    
    yu.cnty <- readr::read_csv(url)
    yu.cnty <- yu.cnty %>% filter(region == get_state_abbrv(State))
    yu.cnty$date <- as.Date(yu.cnty$date, format ="%Y-%m-%d")
    
    msg <- paste0("Successfully download data from Yugang Gu for ", State, " on ", Sys.Date())
    
  } else {
    msg <- paste0("Problem with Yugang Gu link to file updates. Check URL.")
  }
  
  print(msg)
  return(yu.cnty)
  
}