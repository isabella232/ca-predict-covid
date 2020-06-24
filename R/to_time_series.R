to_xts_JHU <- memoize2(function(df,c,measure) {
  d <- df
  j <- d[which(county==c),c(2:10,12)] %>%
       gather(key, value, hosp_occup_mean:cum_deaths_mean) %>%
       spread("intervention", value) %>% as.data.frame()

  j$date <- as.Date(as.POSIXct(j$date), tz = "")

  j.ts <- xts(j[which(j$key == measure), -2],
              j[which(j$key == measure), 1])
  return(j.ts)
})

to_xts_awsJHU <- memoize2(function(df,c,measure) {
  d <- df
  j <-  d[which(county==c),c(1:30)] %>%
    gather(key, value, hosp_occup_mean:cum_deaths_q75 ) %>%
    spread("intervention", value) %>% as.data.frame()

  j.ts <- xts(j[which(j$key == measure), -1],
              j[which(j$key == measure), 1])
  j.ts <- j.ts[,-1]
  return(j.ts)
})


#JHU.ts <- to_xts_JHU(JHU,"Alameda","lowIFR","Deaths")
#rm(j,j.ts)

to_xts_IHME <- memoize2(function(df,c,measure) {
  d <- df
  j <- d[which(d$county==c),c(2,3:29)]

  #j$date <- as.Date(as.POSIXct(j$date), tz = "")

  j.ts <- xts(j[[measure]],
              j[[1]])
  return(j.ts)
})

to_xts_COVID <- memoize2(function(df,c) {
  d <- df
  j <- d[which(d$County.Name==c),c(2,3:10)]

  #j$date <- as.Date(as.POSIXct(j$date), tz = "")

  j.ts <- xts(j[,-1],
              j[[1]])
  return(j.ts)
})

to_xts_CAN <- memoize2(function(df,c,measure) {
  d <- df
  j <- d[which(d$county==c),c(2,3,5:7)] %>%
    gather(key, value, hospitalizations:deaths) %>%
    spread("intervention", value) %>% as.data.frame()

  j.ts <- xts(j[which(j$key == measure), -1],
              j[which(j$key == measure), 1])
  j.ts <- j.ts[,-1]

  return(j.ts)
})

#rm(CAN_sts)

to_xts_RAND <- memoize2(function(df,c,measure) {
  d <- df
  j <- d[which(d$county==c),c(1:11)]  %>%
    gather(key, value, hosp:deaths_upper) %>%
    spread("intervention", value) %>% as.data.frame()

  #j$date <- as.Date(as.POSIXct(j$date), tz = "")

  j.ts <- xts(j[which(j$key == measure), -1],
              j[which(j$key == measure), 1])
  j.ts <- j.ts[,-1]
  return(j.ts)
})
