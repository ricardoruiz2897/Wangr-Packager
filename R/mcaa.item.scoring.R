#'MCAA Item Scoring
#'
#'@description
#'Scores 46 MCAA items by taking character (A/D) and converts to numeric (0/1)
#'
#'@usage
#'mcaa.item.scoring(.df)
#'
#'@param df Dataframe
#'
#'@return
#'Dataframe with columns changed from character (A/D) to numeric (0/1).
#'
#'@examples
#'#Returns dataframe with scored items.
#'df <- mcaa.item.scoring(df)
#'
# mcaa.item.scoring -------------------------------------------------------------
mcaa.item.scoring <- function(df) {
  
  if(!require("dplyr")){
    install.packages("dplyr")
    require("dplyr")
  }

  df <- df %>%  mutate(mcaa1 = dplyr::if_else(mcaa1 == "A", 1, 0),
         mcaa2 = dplyr::if_else(mcaa2 == "A", 1, 0),
         mcaa3 = dplyr::if_else(mcaa3 == "D", 1, 0),
         mcaa4 = dplyr::if_else(mcaa4 == "A", 1, 0),
         mcaa5 = dplyr::if_else(mcaa5 == "A", 1, 0),
         mcaa6 = dplyr::if_else(mcaa6 == "A", 1, 0),
         mcaa7 = dplyr::if_else(mcaa7 == "A", 1, 0),
         mcaa8 = dplyr::if_else(mcaa8 == "D", 1, 0),
         mcaa9 = dplyr::if_else(mcaa9 == "A", 1, 0),
         mcaa10 = dplyr::if_else(mcaa10 == "A", 1, 0),
         mcaa11 = dplyr::if_else(mcaa11 == "A", 1, 0),
         mcaa12 = dplyr::if_else(mcaa12 == "A", 1, 0),
         mcaa13 = dplyr::if_else(mcaa13 == "A", 1, 0),
         mcaa14 = dplyr::if_else(mcaa14 == "A", 1, 0),
         mcaa15 = dplyr::if_else(mcaa15 == "A", 1, 0),
         mcaa16 = dplyr::if_else(mcaa16 == "D", 1, 0),
         mcaa17 = dplyr::if_else(mcaa17 == "A", 1, 0),
         mcaa18 = dplyr::if_else(mcaa18 == "A", 1, 0),
         mcaa19 = dplyr::if_else(mcaa19 == "A", 1, 0),
         mcaa20 = dplyr::if_else(mcaa20 == "A", 1, 0),
         mcaa21 = dplyr::if_else(mcaa21 == "A", 1, 0),
         mcaa22 = dplyr::if_else(mcaa22 == "A", 1, 0),
         mcaa23 = dplyr::if_else(mcaa23 == "A", 1, 0),
         mcaa24 = dplyr::if_else(mcaa24 == "D", 1, 0),
         mcaa25 = dplyr::if_else(mcaa25 == "A", 1, 0),
         mcaa26 = dplyr::if_else(mcaa26 == "A", 1, 0),
         mcaa27 = dplyr::if_else(mcaa27 == "A", 1, 0),
         mcaa28 = dplyr::if_else(mcaa28 == "A", 1, 0),
         mcaa29 = dplyr::if_else(mcaa29 == "A", 1, 0),
         mcaa30 = dplyr::if_else(mcaa30 == "A", 1, 0),
         mcaa31 = dplyr::if_else(mcaa31 == "D", 1, 0),
         mcaa32 = dplyr::if_else(mcaa32 == "D", 1, 0),
         mcaa33 = dplyr::if_else(mcaa33 == "A", 1, 0),
         mcaa34 = dplyr::if_else(mcaa34 == "A", 1, 0),
         mcaa35 = dplyr::if_else(mcaa35 == "A", 1, 0),
         mcaa36 = dplyr::if_else(mcaa36 == "A", 1, 0),
         mcaa37 = dplyr::if_else(mcaa37 == "A", 1, 0),
         mcaa38 = dplyr::if_else(mcaa38 == "A", 1, 0),
         mcaa39 = dplyr::if_else(mcaa39 == "A", 1, 0),
         mcaa40 = dplyr::if_else(mcaa40 == "A", 1, 0),
         mcaa41 = dplyr::if_else(mcaa41 == "A", 1, 0),
         mcaa42 = dplyr::if_else(mcaa42 == "A", 1, 0),
         mcaa43 = dplyr::if_else(mcaa43 == "D", 1, 0),
         mcaa44 = dplyr::if_else(mcaa44 == "A", 1, 0),
         mcaa45 = dplyr::if_else(mcaa45 == "A", 1, 0),
         mcaa46 = dplyr::if_else(mcaa46 == "A", 1, 0))
  
  
  return(df)
  
}
