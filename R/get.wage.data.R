#'Get Wage Data
#'
#'@description
#'The function takes in all the jobs for a person as a tibble and the incarceration end date for the person.
#'Then it returns a tibble where the first job the person had is the first row and the job the person had at
#'one year after first being employed is the second row. If the person was never employed the function returns
#'NULL.
#'
#'@Usage
#'
#'@param jobs a tibble containing all the jobs for a person containing the following columns: "start.date" - the date the jobs started, "dot.code" - the dot code for that job.
#'
#'@param incar.end.date a date object that is the day the person was released from prison.
#'
#'@return
#'The function returns a tibble where the first job the person had is the first row and the job the person had at
#'one year after first being employed is the second row. If the person was never employed the function returns
#'NULL.
#'
#'@example
#'jobs <- data.frame(start.date = list(Date(1-10-2000), Date(1-1-2001)), dot.code = list(1010, 0))
#'incar.end.date = Date(1-1-2000)
#'
#'results <- get.wage.data(jobs, incar.end.date)
#'print(results)
#'
#'output: start.date | dot.code
#'        1-10-2000  | 1010
#'        1- 1-2001  | 0
#'
get.wage.data <- function(jobs, incar.end.date){

  # If either of the inputs were NULL, then return NULL.
  if(is_null(jobs) || is_null(incar.end.date)){
    return(NULL)
  }

  one.year <- incar.end.date + 365  # The date of one year after the end of incarseration.
  bad.dot.codes <- c(0, 997000000, 998000000) # All the dot codes that mean unemployed.

  first.job <- NULL   # The first job the person had within a year of being released.
  one.year.job <- NULL  # The job the person after one year of being employed.
  one.year.of.employment <- NULL  # The date at which the person would have been employed one year.

  # For every job in jobs:
  for(i in 1:nrow(jobs)){
    job <- jobs[i, ]  # Get the job (row of the jobs tibble) to be examined.

    # If the job has a valid start date:
    if(!is.na(job$start.date[1]) && !is_null(job$start.date[1])){

      # If a first job has not yet been found, and this job represents valid employment within one year of being released:
      if(is_null(first.job) && !(job$dot.code[[1]] %in% bad.dot.codes) && (job$start.date[[1]] > incar.end.date) && (job$start.date[[1]] < one.year)){
        first.job <- job  # Set this job to be the first job.
        one.year.job <- job   # Set this job to be the job in one year, in case this is the only job reported before the year is up.
        one.year.of.employment <- job$start.date + 365  # Calculate the one year date from starting this job.
      }

      # Else if the first job is found and this job is within one year of the person starting their first job:
      else if(!is_null(first.job) && job$start.date[[1]] < one.year.of.employment){
        one.year.job <- job  # Set this job to be the job that the person has in one year, in case this is the only job reported before the year is up.
      }
    }
  }

  # If a job within one year of release is not found, then return NULL.
  if(is_null(first.job)){
    return(NULL)
  }
  else{
    return(rbind(first.job, one.year.job)) # Combine the first job and job in one year together as one tibble as described in the description.
  }
}
