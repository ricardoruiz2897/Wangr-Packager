#'Box Plot
#'
#'@description
#'This function takes a list of categorical predictors and continuous outcomes to create a box plot.
#'
#'@Usage
#'box.plot(.dat, cat.out, cont.out, alpha, jitter, filename)
#'
#'@param dat Dataframe to take info from.
#'@param cont.out List of continuous predictors.
#'@param cat.pre List of categorical outcomes.
#'@param alpha
#'@param jitter Jitter Points (Logical)
#'@param filename Name of the file.
#'
#'@return Saves a box plot in working directory on a pdf with the given filename

#'@examples
#'box.plot(data=df, cont.out = continous.outcomes, cat.pre = categorical.predictors, alpha=1, jitter=F, filename="Density Plot")

box.plot <- function(dat, cat.out, cont.pre, alpha=.5, jitter=F, filename, plot.percentage = 100){
  alph <- alpha

  if(missing(dat))
    print("Please supply data")

  if(class(dat)!="data.frame"){
    print("Coercing data to a dataframe")
    dat <- as.data.frame(dat)
  }

  if(jitter==T){
    geomplot <- c("jitter","boxplot")
  } else{
    geomplot <- "boxplot"
  }

  #Substet dataneeded given the wanted percentage percentage.
  percentage <- nrow(dat) * (plot.percentage/100)

  subet.dataneeded <- dat[sample(1:nrow(dat), percentage,
                                 replace=FALSE),]

  dat <- subet.dataneeded

  pdf(paste0(filename,".pdf"))
  for(i in 1:length(cat.out)){
    for(j in 1:length(cont.pre)){

      print(qplot(dat[,cat.out[i]],dat[,cont.pre[j]],data=dat,
                  geom=geomplot,fill=dat[,cat.out[i]],alpha=I(alph),
                  main=paste(colnames(dat)[cat.out[i]],"by",colnames(dat)[cont.pre[j]]),
                  xlab="",ylab=colnames(dat)[cont.pre[j]])
            + guides(fill=guide_legend(title=colnames(dat)[cat.out[i]])))


    }
  }
  dev.off()
}
