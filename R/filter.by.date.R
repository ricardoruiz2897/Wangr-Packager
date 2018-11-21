#'Filter by Date.
#'
#'@description
#'See vignette: vignette("filter.by.date","wangr")
#'
filter.by.date <- function(listname, task, col.name, new.col.name=NA, date1=NA, date2=NA){

  # find.min function -------------------------------------------------------
  find.min <- function(nested.col, name){
    to.return <- purrr::map(nested.col, ~ if(!is.null(.x)) return(min(.x[[name]])) else return(NULL))
    return(to.return)
  }

  # find.max function -------------------------------------------------------
  find.max <- function(nested.col, name){
    to.return <- purrr::map(nested.col, ~ if(!is.null(.x)) return(max(.x[[name]])) else return(NULL))
    return(to.return)
  }

  # Event Function ----------------------------------------------------------
  add.event <- function(nested.col, col.name, new.col.name, event.date){
    tmp5 <- function(date, event.date){

      if(is.na(date) || is.na(event.date)){
        return(NA)
      }

      if(date <= event.date){
        return("pre")
      }
      else{
        return("post")
      }
    }

    tmp4 <- function(tibble, event.date, col.name, new.col.name){
      if(!is.null(tibble)){
        tibble[[new.col.name]] <- unlist(purrr::map(tibble[[col.name]], tmp5, event.date))
        return(tibble)
      }
      else{
        return(NULL)
      }
    }

    if(is_vector(event.date) || is_list(event.date)){
      to.return <- purrr::map2(nested.col, event.date, tmp4, col.name, new.col.name)
    }
    else{
      to.return <- purrr::map(nested.col, tmp4, event.date, col.name, new.col.name)
    }
    return(to.return)
  }

  # Interval function -------------------------------------------------------
  add.interval <- function(nested.col, col.name, new.col.name, event.date.beg, event.date.end){
    tmp7 <- function(date, event.date.beg, event.date.end){
      if(is.na(date) || is.na(event.date.beg) || is.na(event.date.end)){
        return(NA)
      }

      if(date < event.date.beg){
        return("pre")
      }
      else if(date > event.date.end){
        return("post")
      }
      else{
        return("during")
      }
    }

    tmp6 <- function(tibble, col.name, new.col.name, event.date.beg, event.date.end){
      if(!is.null(tibble)){
        tibble[[new.col.name]] <- unlist(purrr::map(tibble[[col.name]], tmp7, event.date.beg, event.date.end))
        return(tibble)
      }
      else{
        return(NULL)
      }
    }

    if((is_vector(event.date.beg) || is_list(event.date.beg)) && (is_vector(event.date.end) || is_list(event.date.end))){
      to.return <- purrr::pmap(list(nested.col, event.date.beg, event.date.end), function(a, b, c) tmp6(a, col.name, new.col.name, b, c))
    }
    else if(is_vector(event.date.beg) || is_list(event.date.beg)){
      to.return <- purrr::pmap(list(nested.col, event.date.beg), function(a, b) tmp6(a, col.name, new.col.name, b, event.date.end))
    }
    else if(is_vector(event.date.end) || is_list(event.date.end)){
      to.return <- purrr::pmap(list(nested.col, event.date.end), function(a, b) tmp6(a, col.name, new.col.name, event.date.beg, b))
    }
    else{
      to.return <- purrr::map(nested.col, tmp6, col.name, new.col.name, event.date.beg, event.date.end)
    }
    return(to.return)
  }


  if(task == "first"){
    return(find.min(listname, col.name))
  }
  else if(task == "last"){
    return(find.max(listname, col.name))
  }
  else if(task == "cut"){
    if(is.na(new.col.name) || is.na(date1)){
      print("ERROR: cut requires new.col.name and date1.")
    }
    else{
      return(add.event(listname, col.name, new.col.name, date1))
    }
  }
  else if(task == "interval"){
    if(is.na(new.col.name) || is.na(date1) || is.na(date2)){
      print("ERROR: cut requires new.col.name, date1 and date2.")
    }
    else{
      return(add.interval(listname, col.name, new.col.name, date1, date2))
    }
  }
  else{
    print("ERROR: task specified is not a valid task.")
  }
}
