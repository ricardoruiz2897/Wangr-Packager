#'Descriptive Table
#'
#'@description
#'This will generate a table that describes columns in a dataframe.
#'change.to.vect.str is a useful function to use with make.desc.tbl
#'
#'@Usage
#'make.desc.tbl(.df, .col.names, .factors.vector, .numerics.vector, name)
#'
#'@param df A dataframe to base the column names to compare.
#'@param col.names Names of the columns from the final table.
#'@param factors.vectors Column names from the dataframe that are factors. Will be rows in resulting dataframe. (Defaults to NULL)
#'@param numerics.vectors Column names from the dataframe that are numerics. Will be rows in resulting dataframe. (Defaults to NULL)
#'@param name Name of the resulting dataframe.
#'
#'@value
#'New dataframe is saved in the enviroment.
#'
#'@examples
#'#normal
#'make.desc.tbl(df, col.names = col.names,
#'              factors.vector = factors.vector,
#'              numerics.vector = numerics.vector)
#'
#'factor null
#'make.desc.tbl(df, col.names = col.names,
#'              factors.vector = NULL,
#'              numerics.vector = numerics.vector,
#'              name = "no.factors")
#'
#'numerics null
#'make.desc.tbl(df,
#'              col.names = col.names,
#'              factors.vector = factors.vector,
#'              numerics.vector = NULL,
#'              name = "no.numerics")
##Function
#Creates descriptive table and saves it to the enviroment.
make.desc.tbl <- function(df, col.names, factors.vector=NULL, numerics.vector=NULL, name="descriptive.table"){

  #Check dependencies.
  if(!require("dplyr")){
    install.packages("dplyr")
    library("dplyr")
  }

  if(!require("purrr")){
    install.packages("purrr")
    library("purrr")
  }

  if(!require("lazyeval")){
    install.packages("lazyeval")
    library("lazyeval")
  }

  if(!require("stats")){
    install.packages("stats")
    library("stats")
  }

  if(!require("htmlTable")){
    install.packages("htmlTable")
    library("htmlTable")
  }

  #Check parameters are there.
  if(is.null(factors.vector) && is.null(numerics.vector)){
    print("At least one argument factors or numerics must be added.")
    return()
  }

  #Create new column names with .true, .false.
  new.col.names <- c()
  for(col in col.names){

    #Get the colname from RDS to know its type to create columns (factor or logical)
    temp <- df %>%
      dplyr::select_(as.name(col))

    if(typeof(temp[[1]])=="logical"){

      new.name.true <- paste(col, ".true", sep = "")
      new.name.false <- paste(col, ".false", sep="")

      new.col.names <- c(new.col.names, new.name.true, new.name.false)

    }else{

      #We know its is a factor.
      factors <- levels(temp[[1]])

      for(fac in factors){

        #Append .true and .false to names.
        new.name <- paste(col, fac, sep = ".")
        new.col.names <- c(new.col.names, new.name)

      }

    }

  }

  #Create descriptive table
  descriptive.table <- data.frame(matrix(ncol=length(new.col.names), nrow=0))
  colnames(descriptive.table) <- new.col.names

  #########################
  #   TOTALS - first row  #
  #########################
  #Returns string of totals with true values for the column name. (1st row in table)
  get.totals <- function(col.name, df){

    temp <- df %>%
      dplyr::select_(as.name(col.name))

    #if logical, else is factor (col.name)
    if(typeof(temp[[1]])=="logical"){

      temp.true <- temp %>%
        dplyr::filter_(interp(~v==T, v=as.name(col.name)))

      temp.false <- temp %>%
        dplyr::filter_(interp(~!is.na(v), v=as.name(col.name))) %>%
        dplyr::filter_(interp(~v==F, v=as.name(col.name)))

      total <- length(df[[1]])

      #totals
      total.true <- length(temp.true[[1]])
      total.false <- length(temp.false[[1]])

      #percents
      percent.true <- (total.true/total) * 100
      percent.true <- round(percent.true, 1)

      percent.false <- (total.false/total) * 100
      percent.false <- round(percent.false, 1)

      total.true <- prettyNum(total.true,big.mark=",",scientific=FALSE)
      total.false <- prettyNum(total.false,big.mark=",",scientific=FALSE)

      #temp.str.true <- paste(as.character(total.true), "; (",
      #                      as.character(percent.true), "% of released offenders.)", sep="")

      temp.str.true <- paste("n = ", as.character(total.true), sep="")

      #temp.str.false <- paste(as.character(total.false), "; (",
      #                       as.character(percent.false), "% of released offenders.)", sep="")

      temp.str.false <- paste("n = ", as.character(total.false), sep="")

      return(c(temp.str.true, temp.str.false))

    }else{

      #Handle factor columns

      #Get factor levels
      factors <- levels(temp[[1]])

      temp.str.v <- c()

      for(fact in factors){

        #Get only rows with that level to get data
        temp1 <- temp %>%
          dplyr::filter_(interp(~v==fact, v=as.name(col.name)))

        #Get results
        total <- length(df[[1]])
        total.true <- length(temp1[[1]])

        #Percents
        percent.true <- (total.true/total) * 100
        percent.true <-round(percent.true, 1)

        total.true <- prettyNum(total.true,big.mark=",",scientific=FALSE)

        #temp.str <- paste(as.character(total.true), "; (",
        #                 as.character(percent.true), "% of released offenders.)", sep="")

        temp.str <- paste("n = ",as.character(total.true), sep="")

        temp.str.v <- c(temp.str.v, temp.str)

      }

      return(temp.str.v)

    }

  }

  #Get the first row
  totals <- purrr::map(col.names, get.totals, df)
  totals <- unlist(totals)

  totals <- as.data.frame(t(totals))
  colnames(totals) <- new.col.names

  descriptive.table <- rbind(descriptive.table, totals)

  rm(totals)

  ################
  # NUMERIC ROWS #
  ################

  #Returns string with value: "Mean: 20.31 Median: 18.6 Standard Deviation: 4.69" (Rows for numerics)
  get.numeric <- function(col.name, df, row.name){

    #We want mean, standard deviation and median
    #filter where current column
    temp <- df %>%
      dplyr::select_(as.name(col.name), as.name(row.name))

    #If logical col.names, else factors
    if(typeof(temp[[1]])=="logical"){

      #True
      temp.true <- temp %>%
        dplyr::filter_(interp(~v==T, v=as.name(col.name))) %>%
        dplyr::filter_(interp(~!is.na(v), v=as.name(row.name)))

      temp.mean.true <- round(mean(temp.true[[2]]),1)
      temp.median.true <- round(median(temp.true[[2]]), 1)
      temp.sd.true <- round(stats::sd(temp.true[[2]]), 1)

      n.true <- prettyNum(length(temp.true[[2]]),big.mark=",",scientific=FALSE)
      temp.mean.true <- prettyNum(temp.mean.true,big.mark=",",scientific=FALSE)
      temp.median.true <- prettyNum(temp.median.true,big.mark=",",scientific=FALSE)
      temp.sd.true <- prettyNum(temp.sd.true,big.mark=",",scientific=FALSE)

      temp.str.true <- paste("n = ", n.true ,"; Mean: ", as.character(temp.mean.true), "; ",
                             " Median: ", as.character(temp.median.true), "; ",
                             " SD: ", as.character(temp.sd.true), "; ",
                             sep="")

      #False
      temp.false <- temp %>%
        dplyr::filter_(interp(~v==F, v=as.name(col.name))) %>%
        dplyr::filter_(interp(~!is.na(v), v=as.name(row.name)))

      temp.mean.false <- round(mean(temp.false[[2]]),1)
      temp.median.false <- round(median(temp.false[[2]]), 1)
      temp.sd.false <- round(stats::sd(temp.false[[2]]), 1)

      n.false <- prettyNum(length(temp.false[[2]]),big.mark=",",scientific=FALSE)
      temp.mean.false <- prettyNum(temp.mean.false,big.mark=",",scientific=FALSE)
      temp.median.false <- prettyNum(temp.median.false,big.mark=",",scientific=FALSE)
      temp.sd.false <- prettyNum(temp.sd.false,big.mark=",",scientific=FALSE)


      temp.str.false <- paste("n = ", n.false ,"; Mean: ", as.character(temp.mean.false), "; ",
                              " Median: ", as.character(temp.median.false), "; ",
                              " SD: ", as.character(temp.sd.false), "; ",
                              sep="")

      return(c(temp.str.true, temp.str.false))

    }else{

      #Handle factors.
      factors <- levels(temp[[1]])

      temp.str.v <- c()

      #For each factor get results in vector
      for(fact in factors){

        #Get only rows with that level to get data
        temp1 <- temp %>%
          dplyr::filter_(interp(~v==fact, v=as.name(col.name))) %>%
          dplyr::filter_(interp(~!is.na(v), v=as.name(row.name)))


        #Get results
        #Keep it with 2 digits
        temp.mean <- round(mean(temp1[[2]]),1)
        temp.median <- round(median(temp1[[2]]), 1)
        temp.sd <- round(stats::sd(temp1[[2]]), 1)

        temp1.total <-  prettyNum(length(temp1[[2]]),big.mark=",",scientific=FALSE)
        temp.mean <- prettyNum(temp.mean,big.mark=",",scientific=FALSE)
        temp.median <- prettyNum(temp.median,big.mark=",",scientific=FALSE)
        temp.sd <- prettyNum(temp.sd,big.mark=",",scientific=FALSE)

        temp.str <- paste("n = ", temp1.total, "; Mean: ", as.character(temp.mean), "; ",
                          " Median: ", as.character(temp.median), "; ",
                          " SD: ", as.character(temp.sd), "; ",
                          sep="")

        temp.str.v <- c(temp.str.v, temp.str)

      }

      return(temp.str.v)

    }

  }

  #Generate and put numerics in descriptive table if user wanted
  if(!is.null(numerics.vector)){

    for(i in 1:length(numerics.vector)){

      #Get numeric rows
      current <- purrr::map(col.names, get.numeric, df, numerics.vector[i])
      current <- unlist(current)

      #Make it a single row df
      current <- as.data.frame(t(current))
      colnames(current) <- new.col.names

      #Bind to main table
      descriptive.table <- rbind(descriptive.table, current)

    }

  }

  ################
  # FACTORS ROWS #
  ################

  #Returns a string with value: for i to Number of factors: "Factor i: value percent"
  get.factors <- function(col.name, df, row.name){

    #filter where current column
    temp <- df %>%
      dplyr::select_(as.name(col.name), as.name(row.name))


    if(typeof(temp[[1]])=="logical"){

      #Trues
      temp.true <- temp %>%
        dplyr::filter_(interp(~v==T, v=as.name(col.name)))

      factors <- factor(temp.true[[2]])
      factor.levels <- levels(factors)

      #Make factors string
      temp.str.true = ""

      for(level in factor.levels){

        #Filter Nas
        temp.true <- temp.true %>%
          dplyr::filter_(interp(~!is.na(v), v=as.name(row.name))) #Made change here

        temp.current <- temp.true %>%
          dplyr::filter_(interp(~v==level, v=as.name(row.name)))

        #Get percent of current level
        percent <- round((nrow(temp.current)/nrow(temp.true)) * 100, 1)

        t <- prettyNum(nrow(temp.current),big.mark=",",scientific=FALSE)

        temp.str.true = paste(temp.str.true, level, ": ", t, "; (", as.character(percent) , "%); ", sep = "")

      }

      #False
      temp.false <- temp %>%
        dplyr::filter_(interp(~v==F, v=as.name(col.name)))

      factors <- factor(temp.false[[2]])
      factor.levels <- levels(factors)

      temp.str.false <- ""

      for(level in factor.levels){

        #Filter Nas
        temp.false <- temp.false %>%
          filter_(interp(~!is.na(v), v=as.name(row.name))) #Made change here

        temp.current <- temp.false %>%
          filter_(interp(~v==level, v=as.name(row.name)))

        #Get percent of current level
        percent <- round((nrow(temp.current)/nrow(temp.false)) * 100, 1)

        t <- prettyNum(nrow(temp.current),big.mark=",",scientific=FALSE)

        temp.str.false = paste(temp.str.false, level, ": ", t, "; (" ,as.character(percent) , "%); ", sep = "")

      }

      return(c(temp.str.true,temp.str.false))

    }else{

      #Upper factors - factors of col.name
      upper.factors <- levels(temp[[1]])

      temp.str.v <- c()

      #Filter Nas
      temp <- temp %>%
        filter_(interp(~!is.na(v), v=as.name(col.name))) #change here

      for(up in upper.factors){

        temp.current.factor <- temp %>%
          filter_(interp(~!is.na(v), v=as.name(col.name))) %>%
          filter_(interp(~v==up, v=as.name(col.name)))

        lower.factors <- levels(as.factor(temp.current.factor[[2]]))

        temp.str <- ""

        for(lower in lower.factors){

          temp.current.factor1 <- temp.current.factor %>%
            filter_(interp(~!is.na(v), v=as.name(row.name))) %>%
            filter_(interp(~v==lower, v=as.name(row.name)))

          #Get percent of current level
          percent <- round((nrow(temp.current.factor1)/nrow(temp.current.factor)) * 100, 1)

          t <- prettyNum(nrow(temp.current.factor1),big.mark=",",scientific=FALSE)

          temp.str = paste(temp.str, lower, ": ", t, "; (", as.character(percent) , "%); ", sep = "")

        }

        temp.str.v <- c(temp.str.v, temp.str)

      }

      return(temp.str.v)

    }

  }

  #Generate factors rows if user wants
  if(!is.null(factors.vector)){

    for(i in 1:length(factors.vector)){

      current <- purrr::map(col.names, get.factors, df, factors.vector[i])
      current <- unlist(current)

      #Make it a single row df
      current <- as.data.frame(t(current))
      colnames(current) <- new.col.names

      #Bind to main table
      descriptive.table <- rbind(descriptive.table, current)

    }

  }

  #Put the first column with names.

  if(is.null(numerics.vector)){

    Names <- c("Sample sizes", factors.vector)

  }else if(is.null(factors.vector)){

    Names <- c("Sample sizes", numerics.vector)

  }else{

    Names <- c("Sample sizes", numerics.vector, factors.vector)

  }

  #Generate totals column. Might be done more efficiently inside each level, but this seems easier.
  temp.char <- prettyNum(nrow(df),big.mark=",",scientific=FALSE)
  temp.char <- paste("n= ", temp.char, sep="")

  totals <- c(temp.char)

  subset.names <- Names[c(2:length(Names))]

  for(sn in subset.names){

    temp <- df %>%
      select_(as.name(sn))

    temp.str <- ""

    if(is.numeric(temp[[1]])){

      #Filter Nas
      temp2 <- temp %>%
        filter_(interp(~!is.na(v), v=as.name(sn)))

      temp.mean <- round(mean(temp2[[1]]),1)
      temp.median <- round(median(temp2[[1]]), 1)
      temp.sd <- round(stats::sd(temp2[[1]]), 1)

      temp.mean <- prettyNum(temp.mean,big.mark=",",scientific=FALSE)
      temp.median <- prettyNum(temp.median,big.mark=",",scientific=FALSE)
      temp.sd <- prettyNum(temp.sd,big.mark=",",scientific=FALSE)

      result <- prettyNum(nrow(temp2),big.mark=",",scientific=FALSE)

      temp.str <- paste("n = ", result, "; Mean: ", as.character(temp.mean), "; Median: ",
                        as.character(temp.median), "; SD: ", as.character(temp.sd),"; ",
                        sep = "")

      totals <- c(totals, temp.str)

    }else{

      factor.levels <- levels(as.factor(temp[[1]]))

      temp.str <- ""

      #Filter Nas
      temp <- temp %>%
        filter_(interp(~!is.na(v), v=as.name(sn)))

      for(level in factor.levels){

        temp.current <- temp %>%
          filter_(interp(~v==level, v=as.name(sn)))

        #Get percent of current level
        percent <- round((nrow(temp.current)/nrow(temp)) * 100, 1)

        t <- prettyNum(nrow(temp.current),big.mark=",",scientific=FALSE)

        temp.str = paste(temp.str, level, ": ", t, " (" ,as.character(percent) , "%); ", sep = "")

      }

      totals <- c(totals, temp.str)

    }
  }

  #Append names and totals as 1st and 2nd row respectively
  descriptive.table$Name <- Names
  descriptive.table$`Full Sample` <- totals

  descriptive.table <- descriptive.table %>%
    select(`Full Sample`, everything()) %>%
    select(Name, everything())

  #View table Format HTML
  htmlTable(descriptive.table, col.columns = c("none", "#F7F7F7"), col.rgroup = c("none", "#F7F7F7"))

  #Save
  assign(name, descriptive.table, envir = .GlobalEnv)

}
#End function
