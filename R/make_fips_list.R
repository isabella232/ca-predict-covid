
make_fips_list <- function(State = "California"){
  
  counties <- get_counties(State)
  fipslist <- as.list(as.character(counties$fips_char))     
  names(fipslist) <- counties$county
  return(fipslist)
}
