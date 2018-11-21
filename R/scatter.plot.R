#'Scatter Plot
#'
#'@description
#'This function takes a list of categorical predictors and a list of continuous outcomes to produce scatter plots.
#'
#'@Usage
#'scatter.plot(.dat, cont.out, cat.pre, alpha=.5, filename)
#'
#'@param dat Dataframe to take info from.
#'@param cont.out List of continuous outcomes
#'@param cat.pre List of categorical Predictors
#'@param alpha
#'@param filename Name of the file
#'
#'@return Saves scatter plots on a pdf with the given filename
#'
#'@examples
#'scatter.plot(data=df, cont.out = continous.outcomes, cat.pre = categorical.predictors, alpha=1, filename="Density Plot")
#'
#Creates Scatterplot.
scatter.plot <- function(dat, cat.pre, cont.pre, cont.out, alpha = .5, filename, plot.percentage = 100){

  alph <- alpha

  if(missing(dat))
    print("Please supply data")

  if(class(dat)!="data.frame"){
    print("Coercing data to a dataframe")
    dat <- as.data.frame(dat)
  }

  #Substet dataneeded given the wanted percentage percentage.
  percentage <- nrow(dat) * (plot.percentage/100)

  subet.dataneeded <- dat[sample(1:nrow(dat), percentage,
                                 replace=FALSE),]

  dat <- subet.dataneeded


  pdf(paste0(filename, ".pdf"))
  for(i in 1:length(cat.pre)){
    for(j in 1:length(cont.pre)){
      for(k in 1:length(cont.out)){

        print(qplot(dat[,cont.out[k]],dat[,cont.pre[j]],
                    data = dat,geom = c("point", "jitter", "smooth"),
                    method = "lm", formula = y~x, color = dat[,cat.pre[i]], fill = dat[,cat.pre[i]], alpha = I(alph),
                    main = paste(colnames(dat)[cont.out[k]], "by\n", colnames(dat)[cont.pre[j]],
                               "\nCategorized by",colnames(dat)[cat.pre[i]]),
                    xlab = colnames(dat)[cont.out[k]], ylab = colnames(dat)[cont.pre[j]]))
        #+ guide_legend(title = colnames(dat)[cont.out[k]])#,
        #+ guides(fill=guide_legend(title=colnames(dat)[cont.out[k]])))

        print(paste("Category", i, "of", length(cat.pre), ". Predictor", j, "of", length(cont.pre), ". Outcome", k, "of", length(cont.out),"."))
      }
    }
  }
  dev.off()
}



