to_xts_JHU <- function(df,c,measure) {
  d <- df
  j <- d[which(county==c),c(2:10,12)] %>%
       gather(key, value, hosp_occup_mean:cum_deaths_mean) %>%
       spread("intervention", value) %>% as.data.frame()

  j$date <- as.Date(as.POSIXct(j$date), tz = "")

  j.ts <- xts(j[which(j$key == measure), -2],
              j[which(j$key == measure), 1])
  return(j.ts)
}

to_xts_awsJHU <- function(df,c,measure) {
  d <- df
  j <-  d[which(county==c),c(1:30)] %>%
    gather(key, value, hosp_occup_mean:cum_deaths_q75 ) %>%
    spread("intervention", value) %>% as.data.frame()

  j.ts <- xts(j[which(j$key == measure), -1],
              j[which(j$key == measure), 1])
  j.ts <- j.ts[,-1]
  return(j.ts)
}


#JHU.ts <- to_xts_JHU(JHU,"Alameda","lowIFR","Deaths")
#rm(j,j.ts)

to_xts_IHME <- function(df,c,measure) {
  d <- df
  j <- d[which(d$county==c),c(2,3:29)]

  #j$date <- as.Date(as.POSIXct(j$date), tz = "")

  j.ts <- xts(j[[measure]],
              j[[1]])
  return(j.ts)
}

to_xts_COVID <- function(df,c) {
  d <- df
  j <- d[which(d$County.Name==c),c(2,3:10)]

  #j$date <- as.Date(as.POSIXct(j$date), tz = "")

  j.ts <- xts(j[,-1],
              j[[1]])
  return(j.ts)
}

to_xts_CAN <- function(df,c,measure) {
  d <- df
  j <- d[which(d$county==c),c(2,3,5:7)] %>%
    gather(key, value, hospitalizations:deaths) %>%
    spread("intervention", value) %>% as.data.frame()

  j.ts <- xts(j[which(j$key == measure), -1],
              j[which(j$key == measure), 1])
  j.ts <- j.ts[,-1]

  return(j.ts)
}

#rm(CAN_sts)

to_xts_RAND <- function(df,c,measure) {
  d <- df
  j <- d[which(d$county==c),c(1:11)]  %>%
    gather(key, value, hosp:deaths_upper) %>%
    spread("intervention", value) %>% as.data.frame()

  #j$date <- as.Date(as.POSIXct(j$date), tz = "")

  j.ts <- xts(j[which(j$key == measure), -1],
              j[which(j$key == measure), 1])
  j.ts <- j.ts[,-1]
  return(j.ts)
}



state_model_xts_memoized <- memoize2(function(c, selected_crosswalk) {
  
  JHU_sts.m <- to_xts_awsJHU(JHU_aws, c,
                             switch(selected_crosswalk,
                                    "1" = "hosp_occup_mean",
                                    "2" = "icu_occup_mean",
                                    "3" = "cum_deaths_mean"
                             ))
  
  JHU_sts.md <- to_xts_awsJHU(JHU_aws, c,
                              switch(selected_crosswalk,
                                     "1" = "hosp_occup_q50",
                                     "2" = "icu_occup_q50",
                                     "3" = "cum_deaths_q50"
                              ))
  colnames(JHU_sts.md) <- paste(colnames(JHU_sts.md),"M", sep = ".")
  
  JHU_sts.L <- to_xts_awsJHU(JHU_aws, c,
                             switch(selected_crosswalk,
                                    "1" = "hosp_occup_q25",
                                    "2" = "icu_occup_q25",
                                    "3" = "cum_deaths_q25"
                             ))
  colnames(JHU_sts.L) <- paste(colnames(JHU_sts.L),"L", sep = ".")
  
  JHU_sts.H <- to_xts_awsJHU(JHU_aws, c, 
                             switch(selected_crosswalk,
                                    "1" = "hosp_occup_q75",
                                    "2" = "icu_occup_q75",
                                    "3" = "cum_deaths_q75"
                             ))
  colnames(JHU_sts.H) <- paste(colnames(JHU_sts.H),"H", sep = ".")
  
  IHME_sts <- to_xts_IHME(IHME,'California',
                          switch(selected_crosswalk,
                                 "1" = "allbed_mean",
                                 "2" = "ICUbed_mean",
                                 "3" = "totdea_mean"
                          ))
  IHME_sts.L <- to_xts_IHME(IHME,'California',
                            switch(selected_crosswalk,
                                   "1" = "allbed_lower",
                                   "2" = "ICUbed_lower",
                                   "3" = "totdea_lower"
                            ))
  IHME_sts.H <- to_xts_IHME(IHME,'California',
                            switch(selected_crosswalk,
                                   "1" = "allbed_upper",
                                   "2" = "ICUbed_upper",
                                   "3" = "totdea_upper"
                            ))
  
  CAN_sts <- to_xts_CAN(CAN_aws, c,
                        switch(selected_crosswalk,
                               "1" = "hospitalizations",
                               "2" = "beds",
                               "3" = "deaths"
                        ))
  
  RAND_sts <- to_xts_RAND(rand_,'California',
                          switch(selected_crosswalk,
                                 "1" = "hosp",
                                 "2" = "icu",
                                 "3" = "deaths"
                          ))
  RAND_sts.L <- to_xts_RAND(rand_,'California',
                            switch(selected_crosswalk,
                                   "1" = "hosp_lower",
                                   "2" = "icu_lower",
                                   "3" = "deaths_lower"
                            ))
  colnames(RAND_sts.L) <- paste(colnames(RAND_sts.L),"L", sep = ".")
  RAND_sts.H <- to_xts_RAND(rand_,'California',
                            switch(selected_crosswalk,
                                   "1" = "hosp_upper",
                                   "2" = "icu_upper",
                                   "3" = "deaths_upper"
                            )) 
  colnames(RAND_sts.H) <- paste(colnames(RAND_sts.H),"H", sep = ".")
  
  COVID_sts <- to_xts_COVID(covid, c)
  
  if (c != "California") {
      all_ts <- suppressWarnings( merge.xts(JHU_sts.m, #New JHU with optional estimates and intervals
                                            JHU_sts.md,
                                            JHU_sts.L,
                                            JHU_sts.H,
                                            CAN_sts,
                                            COVID_sts, fill = NA) )
  } else { 
      all_ts <- suppressWarnings( merge.xts(JHU_sts.m, #New JHU with optional estimates and intervals
                                            JHU_sts.md,
                                            JHU_sts.L,
                                            JHU_sts.H,
                                            IHME_sts, #IHME with intervals
                                            IHME_sts.L,
                                            IHME_sts.H,
                                            CAN_sts,
                                            RAND_sts,
                                            RAND_sts.L,
                                            RAND_sts.H,
                                            COVID_sts, 
                                            fill = NA #Covid outputs
      ) ) 
  }
  
  #all_ts <- all_ts[,c(-1)]
  all_ts <- all_ts["20200301/20201231"]
  return(all_ts)
})
