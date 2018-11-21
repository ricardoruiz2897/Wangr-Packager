#This function changes the column sid for pseudo.id
anon <- function(master.key, df){

  #we use this library to easily decode Pseudo Ids with sid
  if(!require("hashmap")){
    install.packages("hashmap")
    require("hashmap")
  }

  if(!require("dplyr")){
    install.packages("dplyr")
    require("dplyr")
  }

  #create relation sid-pseudo.id
  id.Pid <- hashmap(master.key$id, master.key$pseudo.id)

  #Create pseudo.id column with sid values
  df$pseudo.id <- id.Pid$find(df$id)

  #drop sid column
  df$id <- NULL

  #move Pseudo Id to first column
  df <- df %>%
    select(pseudo.id, everything())

  return(df)

}

#This functions deanonymizes a df
deanon <- function(master.key, df){

  #we use this library to easily decode Pseudo Ids with sid
  if(!require(hashmap)){
    install.packages(hashmap)
    require(hashmap)
  }

  if(!require(dplyr)){
    install.packages(dplyr)
    require(dplyr)
  }

  #create relation pseudo.id-sid
  #Pid.id <- hashmap(master.key$pseudo.id, master.key$id)

  #Create Sid column with pseudo.id values
  #df$id <- Pid.id$find(df$pseudo.id)

  df$id <- master.key$id[match(df$pseudo.id, master.key$pseudo.id)]

  #drop pseudo.id column
  df$pseudo.id <- NULL

  #move Sid to first column
  df <- df %>%
    select(id, everything())

  return(df)

}
