library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs) # Facilitate the clickable icons
library(DT)
library(shiny)
library(readr)
library(stringr)
#library(pool)
#library(DBI)
#library(odbc)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(viridis)
library(lubridate)
library(data.table)
library(dygraphs)
library(xts)
library(ggiraph)
library(plotly)
library(scales)
library(jsonlite)
library(sf)
library(readr)

source("R/import_functions.R")



#Support Functions
source("R/memoize.R")
source("R/add_pop.R")
source("R/to_time_series.R")
source("R/model_output_lists.R")


# Configure memoization using a shared disk cache
diskcache <- diskCache("./cache", max_size = 100 * 1024 * 1024)
memoize2 <- function(fn) {
  memoize(fn, diskcache)
}
# Tell Shiny to also use this cache for renderCachedPlot
shinyOptions(cache = diskcache)


# Setting up all the paths

# PATH CONSTANTS

# Connect Paths to read from
data_path <- "data/approvedCA/data/"

date_updated <- "June 22, 2020"

#### Supporting Data ####

#### read in County and associated FIPS codes
counties <- fread(paste0("data/county_key.csv")) %>% as.data.frame()
cnty.list <- c(unique(as.character(counties$county)))
# counties_sp <- readRDS("data/spatial/counties_sp.RDS") 
counties_sp <- st_read("data/spatial/counties.geojson") %>% mutate(county = NAME_1)

# Create a labeled fips list for counties
fipslist <- as.list(as.character(counties[[2]]))
names(fipslist) <- counties[[1]]
fipslist[1] <- NULL

#### Read in population numbers
cnty.pop <- readr::read_csv(paste0("data/county_pop.csv"))
cnty.pop[,2] <- str_replace_all(cnty.pop[,2],",","")
cnty.pop[2] <- lapply(cnty.pop[2], as.numeric)
cnty.pop <- as_tibble(cnty.pop)

#### County Bed Data ###
cnty.beds <- readr::read_csv( paste0(data_path,"cha_survey/hospital_beds.csv") ) 

#### Actuals ####
covid <- readRDS(paste0(data_path, "covid.rds"))

#### Nowcast/Forecast Data ####

### rt.live ###
rt_live <- fread(paste0(data_path, "rt_live.csv")) %>% mutate(date = as.Date(date))
  
### COVIDActNow Reff ###
can.state.observed <- fread(paste0(data_path,"can_state_reff_table.csv")) %>% mutate(date = as.Date(date))
can.county.observed <-  fread(paste0(data_path,"can_full_reff_table.csv")) %>% mutate(date = as.Date(date))


temp <- data.table(can.county.observed) %>% .[!is.na(RtIndicator), .(fips = unique(fips)), by =.(county)]
canfipslist <- as.list(temp$fips)
names(canfipslist) <- temp$county
rm(temp)

### Epiforecasts ###
epi_forecast <- fread(paste0(data_path,"/epi_forecast.csv")) %>% mutate(date = as.Date(date))

### ICL Rt ###
icl <- fread(paste0(data_path,"/icl_rt.csv")) %>% mutate(date = as.Date(date))

### Yugang Gu Group ### 
# https://github.com/youyanggu/covid19_projections/raw/master/projections/combined/latest_us.csv
yu <- fread(paste0(data_path,"/yuganggu.csv")) %>% mutate(date = as.Date(date)) 

### IHME Proj. ###
IHME <- fread(paste0(data_path,"/ihme.csv")) %>% mutate(date = as.Date(date))

####  Reich Lab 
reich_lab <- fread(paste0(data_path,"/reich_data.csv")) %>% mutate(target_end_date = as.Date(target_end_date))

### MOBS ### 
#https://covid19.gleamproject.org/
mobs <- fread(paste0(data_path,"/mobs.csv")) %>% mutate(date = as.Date(date))

### MIT ###
#https://www.covidanalytics.io/projections
mit <- fread(paste0(data_path,"/mit.csv")) %>% mutate(date = as.Date(Day)) 

### UCLA ###
# "https://gist.githubusercontent.com/knowzou/ecacd65ab863979a9aea0f19a75252c3/raw/us_rt.json"
ucla_state <- fread(paste0(data_path, "/ucla_state.csv")) %>% mutate(date = as.Date(date)) 

ucla_cnty <- get_ucla_cnty_pred("https://gist.githubusercontent.com/ZeroWeight/9a0c53e56c9bf846485a19a324cf74bd/raw/ca_all_pred.json")
ucla_cnty$date <- as.Date(ucla_cnty$date, format ="%m/%d/%y")

# ucla_cnty2 <- fread(paste0(data_path, "/ucla_county.csv")) %>% mutate(date = as.Date(date)) 


ucla_rt <- jsonlite::fromJSON("https://gist.githubusercontent.com/knowzou/ecacd65ab863979a9aea0f19a75252c3/raw/ca_rt.json")$Rt
ucla_names <- names(ucla_rt)
ucla_cnty_rt <- lapply(ucla_names, function(x) { rbindlist(ucla_rt[x]) %>% as.data.frame() %>% gather(date, Rt, 1:length(ucla_rt[[x]])) %>% mutate(county = x) })
ucla_cnty_rt <- rbindlist(ucla_cnty_rt)
ucla_cnty_rt$date <- as.Date(ucla_cnty_rt$date, format ="%m/%d/%y")
ucla_cnty_rt <- ucla_cnty_rt %>% filter(county != "Santa Clara")
rm(ucla_rt, ucla_names)

# ucla_rt_l <- jsonlite::fromJSON("https://gist.githubusercontent.com/knowzou/ecacd65ab863979a9aea0f19a75252c3/raw/ca_rt.json")$lower_Rt
# ucla_names <- names(ucla_rt_l)
# ucla_cnty_rt_l <- lapply(ucla_names, function(x) { rbindlist(ucla_rt_l[x]) %>% as.data.frame() %>% gather(date, Rt_ll, 1:length(ucla_rt_l[[x]])) %>% mutate(county = x) })
# ucla_cnty_rt_l <- rbindlist(ucla_cnty_rt_l)
# ucla_cnty_rt_l$date <- as.Date(ucla_cnty_rt_l$date, format ="%m/%d/%y")
# ucla_cnty_rt_l <- ucla_cnty_rt_l %>% filter(county != "Santa Clara")
# rm(ucla_rt_l, ucla_names)
# 
# ucla_rt_u <- jsonlite::fromJSON("https://gist.githubusercontent.com/knowzou/ecacd65ab863979a9aea0f19a75252c3/raw/ca_rt.json")$upper_Rt
# ucla_names <- names(ucla_rt_u)
# ucla_cnty_rt_u <- lapply(ucla_names, function(x) { rbindlist(ucla_rt_u[x]) %>% as.data.frame() %>% gather(date, Rt_ul, 1:length(ucla_rt_u[[x]])) %>% mutate(county = x) })
# ucla_cnty_rt_u <- rbindlist(ucla_cnty_rt_u)
# ucla_cnty_rt_u$date <- as.Date(ucla_cnty_rt_u$date, format ="%m/%d/%y")
# ucla_cnty_rt_u <- ucla_cnty_rt_u %>% filter(county != "Santa Clara")
# rm(ucla_rt_u, ucla_names)

#ucla_cnty_rt <- merge(ucla_cnty_rt, ucla_rt_l, by  )

### YU Group ### 
# This is really GU group
# https://github.com/youyanggu/covid19_projections/raw/master/projections/combined/latest_us.csv
yu.cnty <- fread(paste0(data_path, "yuganggu_county.csv")) %>% mutate(date = as.Date(date))

####
#### Scenarios ####
####

#### Imperial College London ####

icl_model <- fread(paste0(data_path, "/icl_model.csv")) %>% mutate(date = as.Date(date)) 

#### COVIDActNow ####

can.weak <-  fread(paste0(data_path,"/can_weak_scenario.csv")) %>% 
             mutate(date = as.Date(date),
                    intervention =  "weakDistancingNow") %>% 
             rename(infected = cumulativeInfected,
                    hospitalizations = hospitalBedsRequired,
                    beds = ICUBedsInUse,
                    deaths = cumulativeDeaths) %>%
             left_join(counties, by = c("fips" = "fips")) %>% 
             left_join(cnty.pop, by = c("county" = "county")) %>%
             select(fips, date, intervention, infected, hospitalizations, beds, deaths, pop2020, county) %>%
             rename(totalPopulation = pop2020)


can.strong <-  fread(paste0(data_path,"/can_strong_scenario.csv")) %>% 
               mutate(date = as.Date(date),
                      intervention = "strictDistancingNow") %>% 
               rename(infected = cumulativeInfected,
                       hospitalizations = hospitalBedsRequired,
                       beds = ICUBedsInUse,
                       deaths = cumulativeDeaths) %>%
               left_join(counties, by = c("fips" = "fips")) %>% 
               left_join(cnty.pop, by = c("county" = "county")) %>%
               select(fips, date, intervention, infected, hospitalizations, beds, deaths, pop2020, county) %>%
               rename(totalPopulation = pop2020)

CAN_aws <- rbind(can.weak,can.strong)
can_counties <- unique(CAN_aws$county)

#### JHU Models ####

#Get Bucket
aws_bucket <- list.files(path = paste0(data_path,"awsJHU/")) %>% as.data.frame() %>% rename(Key = 1) %>% mutate(Key = as.character(Key))
#Select County files
aws_bucket_c <- aws_bucket %>% filter(., grepl(".county.",Key)) %>% select(Key)
#Filter to State files
aws_bucket <- aws_bucket %>% filter(., !grepl(".county.",Key)) %>% select(Key)

#aws_bucket <- aws_bucket[c(1:5,10:13),] %>% as.data.frame()
#aws_bucket_c <- aws_bucket_c[c(1:5,10:13),] %>% as.data.frame()

aws_bucket <- aws_bucket[c(1:6,11:14),] %>% as.data.frame()
aws_bucket_c <- aws_bucket_c[c(1:6,11:14),] %>% as.data.frame()

JHU_aws_s <- rbindlist(lapply(aws_bucket[[1]], function(x) get_awsJHU(data_path,x) ))
JHU_aws_c <- rbindlist(lapply(aws_bucket_c[[1]], function(x) get_awsJHU_c(data_path,x) ))

JHU_aws_c <- merge(JHU_aws_c, counties) 
JHU_aws_s$county <- "California"

JHU_aws <- rbind(JHU_aws_s,JHU_aws_c[,2:28])
rm(JHU_aws_c, JHU_aws_s)

JHU_aws <- JHU_aws %>% group_by(county, intervention) %>%
  mutate(cum_deaths_mean = cumsum(new_deaths_mean),
         cum_deaths_q50 = cumsum(new_deaths_q50),
         cum_deaths_q25 = cumsum(new_deaths_q25),
         cum_deaths_q75 = cumsum(new_deaths_q75))

JHU_aws <- JHU_aws %>% ungroup() %>% 
           select(1:25,starts_with("cum_deaths"),everything()) %>% 
           as.data.table()

JHU_inf_end_dt <- "2020-07-13"

JHU_aws <- JHU_aws %>% mutate_at(.,vars(2:21), list(~ ifelse(. == 0 & 
                                                             cum_deaths_mean > 0 &
                                                             date > as.Date(JHU_inf_end_dt),
                                                             NA, .))) %>% data.table()



JHU_aws <- JHU_aws %>% mutate_at(.,vars(2:21), list(~ ifelse(intervention %in% c("Continued_Lockdown",
                                                                                 "Slow-paced_Reopening",
                                                                                 "Moderate-paced_Reopening",
                                                                                 "Fast-paced_Reopening") 
                                                              &  date > as.Date("2020-09-07"),
                                                              NA, .))) %>% data.table()
### RAND Scenarios ###

rand <- fread(paste0(data_path, "rand_scenarios.csv"))

rand_ <- rand %>% filter(scenario %in% c("1-3", "2-2", "3-1")) %>% 
  select(date, 
         current_hospitalizations_num, 
         current_hospitalizations_lower_bound, 
         current_hospitalizations_upper_bound,
         current_icu_num, 
         current_icu_lower_bound, 
         current_icu_upper_bound,
         cumulative_fatalities_num, 
         cumulative_fatalities_lower_bound, 
         cumulative_fatalities_upper_bound,
         scenario) %>% 
  arrange(scenario, date) %>%
  rename(intervention  = scenario) %>%
  mutate(county = "California")
names(rand_) <- c("date",
                      "hosp","hosp_lower","hosp_upper",
                      "icu","icu_lower","icu_upper",
                      "deaths","deaths_lower","deaths_upper",
                      "intervention","county")
rand_$date <- as.Date(rand_$date) 
rand_$intervention <- paste0("rand.",rand_$intervention)
rm(rand)
