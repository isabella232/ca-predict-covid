grab_icl_rt <- function(ST = "CA", State = "California"){

  url <-paste0("https://mrc-ide.github.io/covid19usa/downloads/time-varying-reproduction-number-scenarios.csv")
  
  if (as.character(url_file_exists(url)[1]) == "TRUE" ) {
    
    icl_rt <- readr::read_csv(url) %>% filter(state == ST)
    icl_rt$date <- as.Date(icl_rt$date)
    
    msg <- paste0("Successfully download data from Imperial College London for ", State, " on ", Sys.Date())
    
  } else {
    msg <- paste0("Problem with ICL link to file updates. Check URL.")
  }
  
  print(msg)
  return(icl_rt)
  
}
  