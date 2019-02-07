#'MCAA Scale Scoring
#'
#'@description
#'Scores 4 MCAA subscales and total score
#'
#'@usage
#'mcaa.scale.scoring(.df)
#'
#'@param df Dataframe
#'
#'@return
#'Dataframe with 5 new numeric columns: 4 subscales and total.
#'
#'@examples
#'#Returns dataframe with scored subscales and total score
#'df <- mcaa.scale.scoring(df)
#'
# mcaa.scale.scoring -------------------------------------------------------------
mcaa.scale.scoring <- function(df) {

  if(!require("dplyr")){
    install.packages("dplyr")
    require("dplyr")
  }

  valueForRow <- function(row_sum, missed, can_mis, total_items){

    if(missed == 0){

      return(row_sum)

    } else if(missed > 0 && missed <= can_miss){

      val <- (row_sum / (total_items - missed)) * total_items

      return(val)

    } else{

      return(NA)

    }


  }

  violence.cols <- df %>%
    select(c("mcaa1", "mcaa5", "mcaa9", "mcaa13", "mcaa17", "mcaa21", "mcaa25",
               "mcaa29", "mcaa33", "mcaa37", "mcaa41", "mcaa44"))

  intent.cols <- df %>%
    select(c("mcaa3", "mcaa7", "mcaa11", "mcaa15", "mcaa19", "mcaa23", "mcaa27",
                "mcaa31", "mcaa35", "mcaa39", "mcaa43", "mcaa46"))

  entitlement.cols <- df %>%
    select(c("mcaa2", "mcaa6", "mcaa10", "mcaa14", "mcaa18", "mcaa22", "mcaa26",
               "mcaa30", "mcaa34", "mcaa38", "mcaa42", "mcaa45"))

  associates.cols <- df %>%
    select(c("mcaa4", "mcaa8", "mcaa12", "mcaa16", "mcaa20", "mcaa24", "mcaa28",
               "mcaa32", "mcaa36", "mcaa40"))

  df <- df %>%

    dplyr::mutate(violence.missing = rowSums(is.na(violence.cols))) %>%

    dplyr::mutate(violence = valueForRow(rowSums(violence.cols, na.rm = TRUE),
                                         rowSums(is.na(violence.cols)),
                                         2, 12)) %>%

    dplyr::mutate(intent.missing = rowSums(is.na(intent.cols))) %>%

    dplyr::mutate(intent = valueForRow(rowSums(intent.cols, na.rm = TRUE),
                                       rowSums(is.na(intent.cols)),
                                       2, 12)) %>%

    dplyr::mutate(entitlement.missing = rowSums(is.na(entitlement.cols))) %>%

    dplyr::mutate(entitlement = valueForRow(rowSums(entitlement.cols, na.rm = TRUE),
                                            rowSums(is.na(entitlement.cols)),
                                            2, 12)) %>%

    dplyr::mutate(associates.missing = rowSums(is.na(associates.cols))) %>%

    dplyr::mutate(associates = valueForRow(rowSums(associates.cols, na.rm = TRUE),
                                           rowSums(is.na(associates.cols)),
                                           1, 10)) %>%

    dplyr::mutate(total = violence + intent + entitlement + associates) %>%

    dplyr::mutate(total.missing = (violence.missing + intent.missing + entitlement.missing + associates.missing))

  return(df)

}
