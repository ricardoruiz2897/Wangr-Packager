#'Deid
#'
#'See vignette: vignette("Deid", "wangr")
#'
#'
#This functions takes a new.df and
deid_ <- function(master.key, in.df){

  #we use this library to easily decode Pseudo Ids with sid
  if(!require(hashmap)){
    install.packages(hashmap)
    require(hashmap)
  }

  if(!require(dplyr)){
    install.packages(dplyr)
    require(dplyr)
  }


  #take values which sids we do not have yet.
  notVector <- in.df$id[which(!(in.df$id%in%master.key$id))]
  notVector <- unique(notVector)

  #If there are no values to change, just keep it like that.
  if(length(notVector)==0){

    print("No values to add.")
    return(anon(in.df))

  } else{

    #Get vectors to modify
    id <- c(notVector)

    #Update Pseudo ids
    #get last number
    if(nrow(master.key) > 0){
      low <- master.key$pseudo.id[[length(master.key$pseudo.id)]] + 1
    } else {
      low <- 1
    }

    if(nrow(master.key) > 0){
      upper <- master.key$pseudo.id[[length(master.key$pseudo.id)]] + length(notVector)
    } else {
      upper <- length(notVector)
    }

    pseudo.id <- c(low:upper)
    ndf <- data.frame(id, pseudo.id)

    #this is adding the new values to the master key
    master.key <- rbind(master.key, ndf)

    #update the master.key df with the new values
    assign("master.key", master.key, envir = .GlobalEnv)

    #Create hash relation for quick search
    id.Pid <- hashmap(master.key$id, master.key$pseudo.id)

    #Create Pseudo Id column
    in.df$pseudo.id <- id.Pid$find(in.df$id)

    #Create new.df to merge with in.df
    # df <- filter(in.df, id == notVector)


    new.df <- in.df

    #drop id
    new.df$id <- NULL

    new.df <- new.df %>%
      select(pseudo.id, everything())

    #merge in.df.updates with main.df
    #main.df <- rbind(in.df, df)

  }

  return(new.df)

}


