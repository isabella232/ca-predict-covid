

#### Import Actuals ####

read_hhs_covid <- function(url){
  covid <- read.csv("https://data.chhs.ca.gov/dataset/6882c390-b2d7-4b9a-aefa-2068cee63e47/resource/6cd8d424-dfaa-4bdd-9410-a3d656e1176e/download/covid19data.csv")
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

#### Import JHU ####

get_awsJHU <- function(data_path,fname) {
  
  aws_path <- paste0(data_path,"awsJHU/",fname)
  df <- read.csv(aws_path, stringsAsFactors = FALSE) 
  df$intervention <- str_remove(fname,".csv")
  df <- df %>% rename(date = time,
                      hosp_occup_q50 = hosp_occup_median,
                      hosp_admit_q50 = hosp_admit_median,
                      icu_occup_q50 = icu_occup_median,
                      icu_admit_q50 = icu_admit_median,
                      new_infect_q50 = new_infect_median,
                      new_deaths_q50 = new_deaths_median
  )
  df$date <- as.Date(df$date)
  return(df)
}

get_awsJHU_c <- function(data_path, fname) {
  
  aws_path <- paste0(data_path,"awsJHU/", fname)
  df <- read.csv(aws_path, stringsAsFactors = FALSE) 
  df$intervention <- str_remove(fname,".county.csv")
  df <- df %>% rename(date = time,
                      hosp_occup_q50 = hosp_occup_median,
                      hosp_admit_q50 = hosp_admit_median,
                      icu_occup_q50 = icu_occup_median,
                      icu_admit_q50 = icu_admit_median,
                      new_infect_q50 = new_infect_median,
                      new_deaths_q50 = new_deaths_median
  )
  df <- df %>% rename(fips = geoid)
  df$date <- as.Date(df$date)
  return(df)
  
}

#### Import CAN ####

get_awsCAN <- function(data_path) {
  
  awsCAN <- read.csv( paste0(data_path,"awsCAN/CAN.csv"), stringsAsFactors = FALSE) 
  
  awsCAN <- merge(awsCAN, counties) %>% dplyr::rename(intervention = scenarioNum)
  awsCAN$intervention <- as.character(awsCAN$intervention)
  awsCAN <- awsCAN %>% mutate(intervention=recode(intervention,
                                                  "0" = "Baseline",
                                                  "1" = "strictDistancingNow",
                                                  "3" = "weakDistancingNow"))
  awsCAN$date <- as.Date(awsCAN$date,format = "%m/%d/%y")
  #awsCAN <- as_tibble(awsCAN)
  return(awsCAN)
  
}

get_ucla_cnty_pred <- function(url) {
  ucla <- jsonlite::fromJSON(url)
  ucla_raw <- tibble::enframe(unlist(ucla))
  n_cols_max <- ucla_raw %>% pull(name) %>% str_split("\\.") %>% purrr::map_dbl(~length(.)) %>% max()
  nms_sep <- paste0("name", 1:n_cols_max)
  ucla_cnty <- ucla_raw %>% separate(name, into = nms_sep, sep = "\\.", fill = "right" )
  names(ucla_cnty) <- c("output","type","county","date", "value")
  return(ucla_cnty)
}

get_can_cnty <- function(fips) {
  url <- paste0("https://data.covidactnow.org/latest/us/counties/",paste0("0",fips),".OBSERVED_INTERVENTION.timeseries.json")
  Sys.sleep(2)
  cnty <- jsonlite::fromJSON(url)$timeseries %>% as.data.frame() %>% mutate(date = as.Date(date))
  return(cnty)
}
