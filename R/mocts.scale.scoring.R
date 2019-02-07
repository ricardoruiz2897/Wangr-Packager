#'MOCTS Scale Scoring
#'
#'@description
#'Scores 4 MOCTS subscales and total score
#'
#'@usage
#'mocts.scale.scoring(.df)
#'
#'@param df Dataframe
#'
#'@return
#'Dataframe with 5 new numeric columns: 4 subscales and total.
#'
#'@examples
#'#Returns dataframe with scored subscales and total score
#'df <- mocts.scale.scoring(df)
#'
# mocts.scale.scoring ------------------------------------------------------------
mocts.scale.scoring <- function(df){

  if(!require("dplyr")){
    install.packages("dplyr")
    require("dplyr")
  }

  df$mocts15 <- dplyr::if_else(df$mocts15 == 4, 0, 1)
  df$mocts15 <- dplyr::if_else(is.na(df$mocts15), 1, df$mocts15)

  df$mocts25 <- dplyr::if_else(df$mocts25 == 3, 0, 1)
  df$mocts25 <- dplyr::if_else(is.na(df$mocts25), 1, df$mocts25)


  df$mocts35 <- dplyr::if_else(df$mocts35 == 1, 0, 1)
  df$mocts35 <- dplyr::if_else(is.na(df$mocts35), 1, df$mocts35)

  df$mocts45 <- dplyr::if_else(df$mocts45 == 5, 0, 1)
  df$mocts45 <- dplyr::if_else(is.na(df$mocts45), 1, df$mocts45)

  df$mocts55 <- dplyr::if_else(df$mocts55 == 2, 0, 1)
  df$mocts55 <- dplyr::if_else(is.na(df$mocts55), 1, df$mocts55)

  check80percententgap <- function(row_sum,
                                   max_,
                                   eighty_percent_min,
                                   eighty_percent_max){

    if(row_sum == max_){

      return(1)

    } else if(row_sum >= eighty_percent_min && row_sum <= eighty_percent_max){

      return(row_sum)

    } else{
      return(NA)
    }


  }


  control_var <- c("mocts4", "mocts5" , "mocts6" , "mocts18" , "mocts22" , "mocts23" , "mocts24" ,
    "mocts29" , "mocts30" , "mocts34" , "mocts37", "mocts39", "mocts42",
    "mocts43", "mocts46", "mocts48", "mocts49", "mocts58", "mocts59",
    "mocts61", "mocts62", "mocts64", "mocts65", "mocts67", "mocts69", "mocts70")

  control.cols <- df %>%
    select(control_var)

  cognitive.immaturity_vars <- c("mocts1", "mocts7", "mocts8", "mocts9", "mocts11", "mocts12",  "mocts13",
    "mocts14", "mocts16", "mocts19", "mocts20", "mocts26","mocts27",
    "mocts28", "mocts31", "mocts32", "mocts33", "mocts36", "mocts40",
    "mocts41", "mocts50", "mocts51", "mocts53", "mocts56", "mocts57", "mocts63",
    "mocts66", "mocts68")

  cognitive.immaturity.cols <- df %>%
    select(cognitive.immaturity_vars)

   egocentrism_vars <- c("mocts2", "mocts3", "mocts10", "mocts17", "mocts2", "mocts38", "mocts44",
    "mocts47", "mocts52", "mocts54", "mocts60")

   egocentrism.cols <- df %>%
     select(egocentrism_vars)

  #Control and inattentinveness.
  df <- df %>%
    dplyr::mutate(inattentiveness = (mocts15 + mocts25 + mocts35 + mocts45 + mocts55)) %>%
    dplyr::mutate(non.nas.control = unlist(purrr::map(rowSums(!is.na(control.cols)), check80percententgap, max_=26, 21, 25))) %>%
    dplyr::mutate(control.sum = rowSums(control.cols, na.rm = T)) %>%
    dplyr::mutate(control.average = dplyr::if_else(non.nas.control != 1, (control.sum/non.nas.control)*26, control.sum/non.nas.control)) %>%
    dplyr::mutate(control.missing = rowSums(is.na(control.cols)))

  #cognitive immaturity
  df <- df %>%
    dplyr::mutate(non.nas.cognitive = unlist(purrr::map(rowSums(!is.na(cognitive.immaturity.cols)), check80percententgap, max_ = 28, 22, 27))) %>%
    dplyr::mutate(cognitive.immaturity.sum = rowSums(cognitive.immaturity.cols, na.rm = T))  %>%
    dplyr::mutate(cognitive.immaturity.average = dplyr::if_else(non.nas.cognitive != 1, (cognitive.immaturity.sum/non.nas.cognitive)*28, (cognitive.immaturity.sum/non.nas.cognitive))) %>%
    dplyr::mutate(cognitive.missing = rowSums(is.na(cognitive.immaturity.cols)))

  #Egocentrism and total
  df <- df %>%
    dplyr::mutate(non.nas.egocentrism = unlist(purrr::map(rowSums(!is.na(egocentrism.cols)), check80percententgap, max_ = 11, 9, 10))) %>%
    dplyr::mutate(egocentrism.sum = rowSums(egocentrism.cols, na.rm = T)) %>%
    dplyr::mutate(egocentrism.average = dplyr::if_else(non.nas.egocentrism != 1, (egocentrism.sum/non.nas.egocentrism)*11, (egocentrism.sum/non.nas.egocentrism))) %>%
    dplyr::mutate(egocentrism.missing = rowSums(is.na(egocentrism.cols))) %>%
    dplyr::mutate(total = control.average + cognitive.immaturity.average + egocentrism.average) %>%
    dplyr::mutate(total.missing = egocentrism.missing + cognitive.missing + control.missing)


  df$control <- round(df$control.average, 0)
  df$cognitive.immaturity <- round(df$cognitive.immaturity.average, 0)
  df$egocentrism <- round(df$egocentrism.average, 0)
  df$total <- df$control + df$cognitive.immaturity + df$egocentrism

  #Drop unneccesary
  # df$control.average <- NULL
  # df$non.nas.control <- NULL
  # df$control.sum <- NULL
  # df$non.nas.cognitive <- NULL
  # df$cognitive.immaturity.sum <- NULL
  # df$cognitive.immaturity.average <- NULL
  # df$non.nas.egocentrism <- NULL
  # df$egocentrism.sum <- NULL
  # df$egocentrism.average <- NULL
df <- df %>%
  dplyr::select(-c(control.average, non.nas.control, control.sum, non.nas.cognitive,
                   cognitive.immaturity.sum, cognitive.immaturity.average,
                   non.nas.egocentrism, egocentrism.sum, egocentrism.average))

  return(df)

}


# MOCTS Scoring -----------------------------------------------------------
# Scales: Inattentiveness, Control, Cognitive Immaturity, Egocentrism + Total (without Inattentiveness)

# Inattentiveness:
# mocts15 if 4 then 0 else 1;
# mocts25 if 3 then 0, else 1,
# mocts35 if 1 then 0, else 1;
# mocts45 if 5 then 0, else 1
# mocts55 if 2 then 0, else 1

# inattentiveness
# control:  4, 5, 6, 18, 22, 23, 24, 29, 30, 34, 37, 39, 42, 43, 46, 48, 49, 58, 59, 61, 62, 64, 65, 67, 69, 70 (range 26-130)
# cognitive.immaturity: 1, 7, 8, 9, 11, 12, 13, 14, 16, 19, 20, 26, 27, 28, 31, 32, 33, 36, 40, 41, 50, 51, 53, 56, 57, 63, 66, 68 (range 28-140)
# egocentrism: 2, 3, 10, 17, 21, 38, 44, 47, 52, 54, 60
# total:  sum of Control, Cognitive Immaturity, Egocentrism

#28 for cognitive inmmu 22-25
#11 for egocentrism 9-10
