## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----Create Test Df, include=FALSE---------------------------------------
library(tidyverse)
library(nycflights13)
library(lubridate)

test.df <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n())

fix.min.max <- function(column, min = NA, max = NA, change.min.to = NA, change.max.to = NA){
  
  # Get the class type of the data stored in column.
  type <- class(column[[1]])
  
  # If min is not NA, change all values below min in column to change.min.to.
  if(!is.na(min)){
    column <-  ifelse(column < min, change.min.to, column)
  }
  
  # If max is not NA, change all values above max in column to change.max.to.
  if(!is.na(max)){
    column <-  ifelse(column > max, change.max.to, column)
  }
  
  # If the type of data in the list before was a date, change it to a date.
  if(type == "Date"){
    column <-  as.Date(column, origin="1970-01-01")
  }
  
  return(column)
  
}

## ----Test df, echo=FALSE-------------------------------------------------
test.df

## ----Fix.min.max, echo=TRUE----------------------------------------------
test.df$n <- fix.min.max(test.df$n, 900, 932, NA, NA)


## ----Altered df, echo=FALSE----------------------------------------------
test.df

## ------------------------------------------------------------------------
test.df$date <- fix.min.max(test.df$date, make_date(2013, 01, 04), make_date(2014, 01, 01), NA, NA)


## ----echo=FALSE----------------------------------------------------------
test.df

