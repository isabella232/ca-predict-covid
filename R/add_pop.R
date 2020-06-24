add_pop <- function(df,p){
  
  df[1,38] <- "California"
  df <- merge(df, p, by.x="County_name", by.y="county")
  df <- df[,c(2:38,1,39)]
  return(df)
}


