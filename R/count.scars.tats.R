#'DPS function to count scars and tattoos.
#'
#'@description
#'This function is used in dps1 prep to create column scars and tattoos.
#'
#'@usage
#'count.scars.tats(.df)
#'
#'@param df Should be dps1.
#'
#'@return
#'dps1 is updated with tats and scars columns.
#'
#'@examples
#'#In prep file
#'count.scars.tats(dps1)
#'
# dps functions

# Count Scars and Tattoos -------------------------------------------------
# function to count scars and tattoos in dps1
count.scars.tats <- function(df){

  if(!require("purrr")){
    install.packages("purrr")
    require("purrr")
  }

  if(!require("dplyr")){
    install.packages("dplyr")
    require("dplyr")
  }

  if(!require("stringr")){
    install.packages("stringr")
    require("stringr")
  }

  smt.var.names <- c("smt", "smt1", "smt2", "smt3", "smt4",
                     "smt5", "smt6", "smt7", "smt8", "smt9")
  scars.var.names <- c("scars0", "scars1", "scars2", "scars3", "scars4",
                       "scars5", "scars6", "scars7", "scars8", "scars9")
  tats.var.names <- c("tats0", "tats1", "tats2", "tats3", "tats4",
                      "tats5", "tats6", "tats7", "tats8", "tats9")

  # create 10 variables named scars0-9 & keep values from smt-smt9
  df[paste0("scars", 0:9)] <- df[smt.var.names]
  # in 10 new scars variables, count if "SC" is found in variable
  df[scars.var.names] <- purrr::map(df[scars.var.names], str_count, "SC")

  # create 10 variables named tats0-9 & keep values from smt-smt9
  df[paste0("tats", 0:9)] <- df[smt.var.names]
  df[tats.var.names] <- purrr::map(df[tats.var.names], str_count, "TAT")

  # sum number of scars in each of the scars0-9 variables
  # and tatoos in tats0-9 variables
  # then delete intermediate scars0-9 and tats0-9 variables
  df <- df %>%
    dplyr::mutate(scars = rowSums(df[grep("scars[0-9]",
                                          names(.))], na.rm = TRUE),
                  tats = rowSums(df[grep("tats[0-9]",
                                         names(.))], na.rm = TRUE)) %>%
    dplyr::select(-c(scars0:scars9, tats0:tats9, smt:smt9))

  return(df)

}

#count.scars.tats(dps1)

