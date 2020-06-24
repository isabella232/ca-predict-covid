
grab_mobs <- function(State = "California"){
  url <- paste0("https://data-tracking-api-dot-mobs-2019-ncov-web.appspot.com/data?state=",State,"&frequency=daily")
  
  
    tryCatch({
      mobs <- jsonlite::fromJSON(url)
      mobs <- unique(mobs)
      msg <- paste0("Successfully download data from MOBS for ", State, " on ", Sys.Date())
      print(msg)
      return(mobs)
      
      }, error=function(e){ 
        


        mobs <- tbl(covid_db, sql("SELECT JSON_CONTENT FROM COVID.CDPH.VW_MODELING_MOBS_CURRENT")) %>%
          collect() 
        mobs <- jsonlite::fromJSON(mobs$JSON_CONTENT)
        mobs <- unique(mobs)
        mobs <-  mobs[[12]]
        msg <- paste0("Grabbed MOBS from Snowflake")
    
# 
#       mobs <- jsonlite::fromJSON(paste0("saved_old_versions/mobs_",State,"_data.json"))
#         mobs <- unique(mobs)
#         
#         
        # msg <- paste0("Problem with MOBS link to file update. Grabbing old copy (June 6, 2020).")
#      
     print(msg)
     return(mobs)
      })
    
  

}

