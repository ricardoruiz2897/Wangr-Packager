#'RCI
#'
#'@description
#'This function calculates RCI and creates a graph of the results.
#'
#'@Usage
#'rci(data, "a", "b", 0.7, FALSE, "column", "Results Graph")
#'
#'@param data the data set
#'
#'@param pretest the column name for pretest
#'
#'@param posttest the column name for posttest
#'
#'@param reliability the reliablity score (the number should be less than 1)
#'
#'@param increase True if the increase is good, otherwise False meaning degrease is good
#'
#'@param name a string that will be used to create column names to specify which test the rci/difference result and test differences are for
#'
#'@param graph.title a string that is the title that will appear on the graph
#'
#'@param plot.percentage a number (1-100) of the percentege of point to be plotted
#'
#'@param categories number of categories to be provided in the key. Should only be 3 or 7.
#'
#'@return
#'the results of the function as well as a graph that is created
#'
rci <- function(data,
                pretest,
                posttest,
                reliability,
                increase=FALSE,
                name,
                graph.title=NA,
                plot.percentage=100,
                categories = 7) {


  if(reliability < 0 | reliability > 1) {
    print("Reliability must be between 0 and 1.")
    return(NULL)
  }

  if(categories != 3 && categories != 7){
    print("Categories can only be 3 or 7.")
    return(NULL)
  }

  if(!require("dplyr")){
    install.packages("dplyr")
    require("dplyr")
  }

  if(!require("ggplot2")){
    install.packages("ggplot2")
    require("ggplot2")
  }

  # Creates the text for the key in the RCI function.
  create.key.text <- function(data, categories){

    # Get the group results from the RCI return data passed in.
    group.data <- data$group.results

    line <- gettextf("N = %s; RCI = %.2f", prettyNum(group.data$sample_size, big.mark=",", scientific=FALSE),
                     group.data$RCI)
    space <- " "

    if(categories == 7){

      line.two <- gettextf("%.1f%% Significantly Improved", (group.data$Improved/group.data$sample_size) * 100)
      line.three <- gettextf("%.1f%% Moderately Improved", (group.data$Recovered/group.data$sample_size) * 100)
      line.four <- gettextf("%.1f%% Mildly Improved", (group.data$Remitted/group.data$sample_size) * 100)
      line.five <- gettextf("%.1f%% Unchanged", (group.data$NoChange/group.data$sample_size) * 100)
      line.six <- gettextf("%.1f%% Mildly Deteriorated", (group.data$MildDeter/group.data$sample_size) * 100)
      line.seven <- gettextf("%.1f%% Moderately Deteriorated", (group.data$ModeDeter/group.data$sample_size) * 100)
      line.eight <- gettextf("%.1f%% Deteriorated", (group.data$Deteriorated/group.data$sample_size) * 100)

      key.text <- paste(line, line.two, line.three, line.four, line.five, line.six, line.seven,
                        line.eight, sep = "\n")

    }else{

      line.one <- gettextf("%.1f%% Improved", (group.data$Improved/group.data$sample_size) * 100)
      line.two <- gettextf("%.1f%% Unchanged", (group.data$NoChange/group.data$sample_size) * 100)
      line.three <- gettextf("%.1f%% Deteriorated", (group.data$Deteriorated/group.data$sample_size) * 100)


      key.text <- paste(line, space, line.one, line.two, line.three, sep = "\n")
    }

    return(key.text)

  }

  # Counts the results in the data in the RCI function.
  add.counts <- function(data, results, categories){

    if(categories == 7){

      results$Recovered = nrow(data[which(data$difference.result == "Recovered"),])
      results$Remitted = nrow(data[which(data$difference.result == "Remitted"),])
      results$Improved = nrow(data[which(data$difference.result == "Improved"),])
      results$Deteriorated = nrow(data[which(data$difference.result == "Deteriorated"),])
      results$ModeDeter = nrow(data[which(data$difference.result == "Moderately Deteriorated"),])
      results$MildDeter = nrow(data[which(data$difference.result == "Mildly Deteriorated"),])
      results$NoChange = nrow(data[which(data$difference.result == "No Change"),])

    }

    #Only looking for 3 categories.
    else{

      results$Improved = nrow(data[which(data$difference.result == "Improved"),])
      results$Deteriorated = nrow(data[which(data$difference.result == "Deteriorated"),])
      results$NoChange = nrow(data[which(data$difference.result == "No Change"),])

    }


    return(results)
  }

  # Renames the columns in data in the RCI function.
  rename.by.last <- function(data, name){

    colnames(data)[4] = paste(name, colnames(data)[4], sep=".")
    colnames(data)[5] = paste(name, colnames(data)[5], sep=".")
    colnames(data)[6] = paste(name, colnames(data)[6], sep=".")
    return(data)

  }

  ## Creates a dataset that contains the columns pseudo_id, pretest, postest,
  ## and difference (which is the differnce in score of the pre and post tests)
  dataneeded <- data %>%
    select(1, pretest, posttest) %>%
    dplyr::mutate(difference = .[[3]] - .[[2]]) %>%
    dplyr::filter(!is.na(difference))

  if(nrow(dataneeded) == 0){
    print("Error :: When NA data were removed, there were no data left to plot.")
    return(NULL)
  }


  ## creats a dataset of basic statistics of the dataneeded dataset.
  results <- data.frame(sample_size = nrow(dataneeded), premean = mean(dataneeded[[2]], na.rm=T),
                        preSD = sd(dataneeded[[2]], na.rm=T), postmean = mean(dataneeded[[3]], na.rm = T),
                        postSD = sd(dataneeded[[3]], na.rm=T))

  ## add Standard Error, RCI.CI,
  results$SE=(results$preSD * sqrt(1-reliability))
  results$RCI.CI = 1.96 * sqrt(2*results$SE * results$SE)
  results$RCI = (results$premean - results$postmean)/results$SE


  #add RCI column to dataneeded dataset
  dataneeded$RCI <- (dataneeded$difference)/results$SE[1]

  ninety <- 1.28 * sqrt(2 * results$SE * results$SE)
  eighty <- .84 * sqrt(2 * results$SE * results$SE)

  if(increase) {

    if(categories == 7){

      #Caetgorizes people into one of 7 categories based on the improved/detioration of post test scores.
      dataneeded$difference.result <- ifelse(dataneeded$difference > results$RCI.CI[1], "Recovered",
                                             ifelse(dataneeded$difference > ninety, "Remitted",
                                                    ifelse(dataneeded$difference > eighty, "Improved",
                                                           ifelse(dataneeded$difference * -1 > results$RCI.CI[1], "Deteriorated",
                                                                  ifelse(dataneeded$difference * -1 > ninety,"Moderately Deteriorated",
                                                                         ifelse(dataneeded$difference * -1 > eighty, "Mildly Deteriorated",
                                                                                "No Change"))))))

    }

    #Else categories equals 3
    else{

      #Caetgorizes people into one of 7 categories based on the improved/detioration of post test scores.
      dataneeded$difference.result <- ifelse(dataneeded$difference > results$RCI.CI[1], "Improved",
                                             ifelse(dataneeded$difference > ninety, "Improved",
                                                    ifelse(dataneeded$difference > eighty, "Improved",
                                                           ifelse(dataneeded$difference * -1 > results$RCI.CI[1], "Deteriorated",
                                                                  ifelse(dataneeded$difference * -1 > ninety,"Deteriorated",
                                                                         ifelse(dataneeded$difference * -1 > eighty, "Deteriorated",
                                                                                "No Change"))))))

    }


    # Count the different group results.
    results <- add.counts(dataneeded, results, categories)

    ### renaming based on the last input of the function
    dataneeded <- rename.by.last(dataneeded, name)

    # Create the points on the plot with dataneeded (pre vs. post).
    plot <- ggplot(dataneeded, aes_string(names(dataneeded)[2],  names(dataneeded)[3])) +
      geom_jitter(size=1, alpha = 0.4, position = position_jitter(width = 0.4, height = 0.4),  color = ifelse(dataneeded[4] > eighty, 'green4',
                                                                                                              ifelse(dataneeded[4] * -1 > eighty, 'red', 'black')))
  }
  ## Lower Score on post test is better.
  else{

    if(categories == 7){

      dataneeded$difference.result <- ifelse(dataneeded$difference > results$RCI.CI[1], "Deteriorated",
                                             ifelse(dataneeded$difference > ninety, "Moderately Deteriorated",
                                                    ifelse(dataneeded$difference > eighty, "Mildly Deteriorated",
                                                           ifelse(dataneeded$difference * -1 > results$RCI.CI[1], "Recovered",
                                                                  ifelse(dataneeded$difference * -1 > ninety,"Remitted",
                                                                         ifelse(dataneeded$difference * -1 > eighty, "Improved",
                                                                                "No Change"))))))
    }else{

      dataneeded$difference.result <- ifelse(dataneeded$difference > results$RCI.CI[1], "Deteriorated",
                                             ifelse(dataneeded$difference > ninety, "Deteriorated",
                                                    ifelse(dataneeded$difference > eighty, "Deteriorated",
                                                           ifelse(dataneeded$difference * -1 > results$RCI.CI[1], "Improved",
                                                                  ifelse(dataneeded$difference * -1 > ninety,"Improved",
                                                                         ifelse(dataneeded$difference * -1 > eighty, "Improved",
                                                                                "No Change"))))))

    }


    # Count the different group results.
    results <- add.counts(dataneeded, results, categories)

    # Rename to columns according to the parameter passed in, name.
    dataneeded <- rename.by.last(dataneeded, name)

    #Substet dataneeded given the wanted percentage percentage.
    percentage <- nrow(dataneeded) * (plot.percentage/100)

    subet.dataneeded <- dataneeded[sample(1:nrow(dataneeded), percentage,
                                          replace=FALSE),]

    # Create the points on the plot.
    plot <- ggplot(subet.dataneeded, aes_string(names(subet.dataneeded)[2],  names(subet.dataneeded)[3])) +
      geom_jitter(size=1, alpha = 0.4, position = position_jitter(width = 0.4, height = 0.4),
                  color = ifelse(subet.dataneeded[4] > eighty,'red' , ifelse(subet.dataneeded[4] * -1 > eighty, 'green4', 'black')))
  }

  finalmerge <- select(dataneeded, 1, 4, 5, 6)

  dataset <- left_join(data, finalmerge, by.x = 1)

  # Create the text for the key.
  key.text <- create.key.text(data.to.return, categories)

  # Create the rest of the plots.
  plot_obj <- plot +
    xlab("Pre-test") +
    ylab("Post-test") +
    ggtitle(graph.title) +
    geom_abline(slope = 1, intercept = 0) +
    geom_abline(slope = 1, intercept = 0 + results$RCI.CI[1], color="black", linetype="dashed") +
    geom_abline(slope = 1, intercept = 0 - results$RCI.CI[1], color="black", linetype="dashed") +
    geom_abline(slope = 1, intercept = 0 + ninety, color="black", linetype="dashed") +
    geom_abline(slope = 1, intercept = 0 - ninety, color="black", linetype="dashed") +
    geom_abline(slope = 1, intercept = 0 + eighty, color="black", linetype="dashed") +
    geom_abline(slope = 1, intercept = 0 - eighty, color="black", linetype="dashed") +
    geom_text(aes(y = 10.15, x = 10, label = "No Change")) +
    #geom_text(aes(y = 10.15, x = 10 - eighty, label = ".80")) +
    #geom_text(aes(y = 10.15, x = 10 - ninety, label = ".90")) +
    #geom_text(aes(y = 10.15, x = 10 - results$RCI.CI[1], label = ".95")) +
    #geom_rect(aes(x=-Inf,y=Inf,hjust=0,vjust=1, color="white")) +
    geom_label(aes(x=-Inf,y=Inf,hjust=0,vjust=1,label=key.text), fill="white", family="Times New Roman", lineheight = 0.85)
#    labs(caption = "The confidence intervals at at 80%, 90% and 95%.", family="Times New Roman")

  print(plot_obj)

  # Create the data to be returned.
  data.to.return <- list(individual.results = dataneeded, group.results = results, plot = plot_obj)

  ## returns the original dataset input with difference, rci and category added, specific test dataset (dataneeded),
  ## and basic stats about dataset(results)
  return(data.to.return)
}
