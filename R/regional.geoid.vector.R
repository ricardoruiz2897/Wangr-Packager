#'Regional Geoid Vector
#'
#'@description
#'This function will return a vector of numeric geoid values.
#'
#'@Usage
#'regional.geoid.vector(df, geoid.col.name, region.col.name, region.number, vector.name)
#'
#'@param df A dataframe that contains the information. Defaults to "tx.co.geometry" (Dataframe)
#'@param geoid.col.name Name of column that contains the vector of fips. Defaults to "geoid" (String)
#'@param region.col.name Name of column that contains the regions number. Defaults to "dfps.region" (String)
#'@param region.number Number from the region we want to filter by. (Number)
#'@param vector.name Name of the resulting vector. Defaults to "default.vector.name" (String)
#'
#'@value
#'New vector is saved in the enviroment.
#'
#'@examples
#'regional.geoid.vector(tx.co.geometry, "geoid", "dfps.region", 2, "region2.vector")
#'
#'
regional.geoid.vector <- function(df = tx.co.geometry, 
                                  geoid.col.name = "geoid", 
                                  region.col.name = "dfps.region", 
                                  region.number, 
                                  vector.name = "default.vector.name"){
  
  if(!require("dplyr")){
    install.packages("dplyr")
    require("dplyr")
  }
  
  if(!require("lazyeval")){
    install.packages("lazyeval")
    require("lazyeval")
  }
  
  df <- df %>%
    dplyr::filter_(interp(~v==region.number , v=as.name(region.col.name)))
  
  temp1 <- df[[geoid.col.name]]
  
  temp.vector <- c()
  
  for(i in 1:nrow(df)){
    
    v <- toString(temp1[i])
    temp.vector <- c(temp.vector, v)
    
  }
  
  assign(vector.name, temp.vector, envir = .GlobalEnv)
  
}