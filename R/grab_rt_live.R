
grab_rt_live <- function(State = "California"){

  ST = get_state_abbrv(State)
  
  url <- "https://d14wlfuexuxgcm.cloudfront.net/covid/rt.csv"
 
  if ( as.character(url_file_exists(url)[1]) == "TRUE" ) {
    rt_live <- readr::read_csv("https://d14wlfuexuxgcm.cloudfront.net/covid/rt.csv") %>% 
      filter(region == ST) %>% 
      mutate(date = as.Date(as.character(date)),
             region = as.character(region))
    
    msg <- paste0("Successfully download data from Rt.live for ", State, " on ", Sys.Date())
    
  } else {
    msg <- paste0("Problem with Rt.live link to file updates. Check URL.")
  }
  
  print(msg)
  return(rt_live)
}