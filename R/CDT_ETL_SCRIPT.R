
library(readr)
library(stringr)
library(pool)
library(DBI)
library(odbc)
library(pool)
library(dplyr)
library(tidyr)
library(forcats)
library(lubridate)
library(data.table)
library(dygraphs)
library(xts)
library(httr)
library(jsonlite)
library(curl)
library(openssl)



#### Supporting Data ####

#### read in County and associated FIPS codes

counties <- fread(paste0("data/county_key.csv")) %>% 
  as.data.frame()

# Create a labeled fips list for counties

fipslist <- as.list(as.character(counties[[2]]))
names(fipslist) <- counties[[1]]
fipslist[1] <- NULL

#### Actuals ####

url <- "https://data.chhs.ca.gov/dataset/6882c390-b2d7-4b9a-aefa-2068cee63e47/resource/6cd8d424-dfaa-4bdd-9410-a3d656e1176e/download/covid19data.csv"

read_hhs_covid <- function(url){
  covid <- readr::read_csv("https://data.chhs.ca.gov/dataset/6882c390-b2d7-4b9a-aefa-2068cee63e47/resource/6cd8d424-dfaa-4bdd-9410-a3d656e1176e/download/covid19data.csv")
  covid <- covid[,1:8]
  covid[,1:2] <- lapply(covid[,1:2], as.character)
  covid$Most.Recent.Date <- mdy(covid$Most.Recent.Date)
  covid$total.hospital <- covid$COVID.19.Positive.Patients + covid$Suspected.COVID.19.Positive.Patients
  covid$total.icu <-  covid$ICU.COVID.19.Positive.Patients + covid$ICU.COVID.19.Suspected.Patients
  covid_s <- aggregate(covid[,3:10],by=list(covid$Most.Recent.Date), sum, na.rm = TRUE)
  covid_s <- covid_s %>% rename(Most.Recent.Date = Group.1)
  covid_s$County.Name <- "California"
  covid <- rbind(covid,covid_s[,c(10,1:9)])
  return(covid)
}

urlFileExist <- function(url){
  HTTP_STATUS_OK <- c(200,302)
  hd <- httr::HEAD(url)
  status <- hd$all_headers[[1]]$status
  list(exists = status %in% HTTP_STATUS_OK, status = status)
}

if ( as.character(urlFileExist(url)[1]) == "TRUE" ) {
  covid <- read_hhs_covid( url )
  saveRDS(covid,"data/covid.rds")
} else {
  covid <- source( "data/covid.rds" )
}


### Rt.live ####

rt_live <- readr::read_csv("https://d14wlfuexuxgcm.cloudfront.net/covid/rt.csv") %>% 
              filter(region == "CA") %>% 
              mutate(date = as.Date(as.character(date)),
                     region = as.character(region))

### COVIDActNow ###

can.ca <- jsonlite::fromJSON("https://data.covidactnow.org/latest/us/states/CA.OBSERVED_INTERVENTION.timeseries.json")$timeseries %>% 
                as.data.frame() %>% 
                mutate(date = as.Date(date))

get_can_cnty <- function(fips) {
  
  url <- paste0("https://data.covidactnow.org/latest/us/counties/",paste0("0",fips),".OBSERVED_INTERVENTION.timeseries.json")
  Sys.sleep(2)
  valid <- RCurl::url.exists(url)
  print(paste0(fips,": ",valid))
  if(valid){
    cnty <- jsonlite::fromJSON(url)$timeseries %>% as.data.frame() %>% mutate(date = as.Date(date))
    cnty <- mutate(cnty, fips = fips)
    return(cnty)
  }
}

# CAN county estimates
out <- rbindlist(lapply(fipslist, function(x) get_can_cnty(x)))
full_can_table <- mutate(out, fips = as.integer(fips)) %>% merge(regions, all.x = TRUE)

fwrite(full_can_table, "../data/spatial/can_full_table.csv")

### Epiforecasts ###

epifc_url <-"https://github.com/epiforecasts/covid-regional/raw/ada5b4ec0a5e786712c630708aaf85de663e2dde/united-states/regional-summary/rt.csv"
epi_forecast <- readr::read_csv(epifc_url) %>% 
                  filter(region == "California") %>% 
                  mutate(date = as.Date(date))

### IHMEproj###
ihme_url <- "https://ihmecovid19storage.blob.core.windows.net/latest/ihme-covid19.zip"

if ( as.character(urlFileExist(ihme_url)[1]) == "TRUE" ) {
  
  temp <- tempfile()
  temp2 <- tempfile()
  download.file(ihme_url,temp)
  unzip(zipfile = temp, exdir = temp2)
  folder <- list.files(temp2)
  file.copy(file.path(temp2, paste0(folder,"/Hospitalization_all_locs.csv") ), 
            paste0(data_path,"IHME/Hospitalization_all_locs.csv")
  )
  file.rename(paste0(data_path,"IHME/Hospitalization_all_locs.csv"),
              paste0(data_path,"IHME/Hospitalization_all_locs","_",folder,".csv")
  )
  file.copy(file.path(temp2, paste0(folder,"/Hospitalization_all_locs.csv") ), 
            paste0(data_path,"IHME/Hospitalization_all_locs.csv")
  )
  unlink(c(temp, temp2))
  ihme_msg <- paste0("File was updated; new projections for ",folder,". Current files:")
  
} else {
  ihme_msg <- paste0("Problem with link to file updates. Check URL.")
}

ihme_files <- list.files( paste0(data_path,"IHME/") )
IHME <- fread( paste0(data_path,"IHME/Hospitalization_all_locs.csv") ) # Redirected to local

IHME <- IHME[which(IHME$location_name == "California"),]
IHME <- IHME  %>% 
          rename(county = location_name) %>% 
          select(-1)
IHME$date <- as.Date(IHME$date)

### LANL 

## Cumualative Cases - Date will vary...so far, do not know how often these are updated.
LANL <- readr::read_csv("https://covid-19.bsvgateway.org/forecast/us/files/2020-05-03/confirmed/2020-05-03_confirmed_quantiles_us_website.csv") %>%
            filter(simple_state == "california") %>% select(dates,q.10,q.50,q.90, truth_confirmed)

LANL_d <- readr::read_csv("https://covid-19.bsvgateway.org/forecast/us/files/2020-05-03/deaths/2020-05-03_deaths_quantiles_us_website.csv") %>%
            filter(simple_state == "california") %>% select(dates,q.10,q.50,q.90, truth_confirmed)


### MOBS ### 
#https://covid19.gleamproject.org/

mobs_url <- "https://data-tracking-api-dot-mobs-2019-ncov-web.appspot.com/data?state=California&frequency=daily"
mobs <- jsonlite::fromJSON(mobs_url)
mobs <- unique(mobs)
mobs$date <- as.Date(mobs$date)

### MIT ###
#https://www.covidanalytics.io/projections
mit <- readr::read_csv("https://www.covidanalytics.io/projections/covid_analytics_projections.csv") %>% 
          filter(Province == "California")
mit$date <- as.Date(mit$Day)


### UCLA ###
# "https://gist.githubusercontent.com/knowzou/ecacd65ab863979a9aea0f19a75252c3/raw/us_rt.json"
ucla <- jsonlite::fromJSON("https://gist.githubusercontent.com/knowzou/ecacd65ab863979a9aea0f19a75252c3/raw/us_rt.json")
ucla_ca <- rbindlist(ucla$Rt["US-CA"])
ucla_ca <- ucla_ca %>% 
            gather(date, Rt, 1:147)
ucla_ca$date <- as.Date(ucla_ca$date, format = "%m/%d/%y")
rm(ucla)

### youyanggu ### 

# https://github.com/youyanggu/covid19_projections/raw/master/projections/combined/latest_us.csv
yu <- readr::read_csv("https://github.com/youyanggu/covid19_projections/raw/master/projections/combined/latest_us.csv") %>% 
          filter(region == "CA")   
yu$date <- as.Date(yu$date, format ="%Y-%m-%d")


#### ICL Imperial College London ####

icl_model <- readr::read_csv("https://mrc-ide.github.io/covid19usa/downloads/data-model-estimates.csv") %>% filter(state == "CA")
icl_model$date <- as.Date(icl_model$date)
icl <- readr::read_csv("https://mrc-ide.github.io/covid19usa/downloads/time-varying-reproduction-number-scenarios.csv") %>% filter(state == "CA")
icl$date <- as.Date(icl$date)

