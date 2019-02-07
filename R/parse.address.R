#'
#'@title Parse Address
#'
#'@param  df A dataframe with the information.
#'@param  col Column with the address or geocode
#'
#'@return A dataframe with the new geocode related columns.
#'@export
#'
#'@examples
#'df <- parse.address(df, col)
#'


parse.address <- function(df, col) {

  #Returns dataframe
  process.geocode <- function(df, col){

    geo.code <- c()
    latitude <- c()
    longitude <- c()

    geo.code <- c(geo.code)

    lat <- as.character(unlist(str_split(geo, pattern = ","))[1])
    long <- as.character(unlist(str_split(geo, pattern = ","))[2])

    lat <- str_sub(lat, 2, nchar(lat))
    long <- str_sub(long, 1, nchar(long)-1)

    latitude <- c(latitude, lat)
    longitude <- c(longitude, long)

    df$latitude <- latitude
    df$longitude <- longitude

    return(df)

  }

  #Returns vector
  processMailing <- function(mailing){

    last <- nchar(mailing)
    current <- ""
    n <- 0
    while(current != "-"){

      n <- n + 1
      current <- str_sub(mailing, n, n)

    }

    #n is location of "-"
    zip.code <- substr(mailing, n+2, last)
    st <- substr(mailing, n-2, n-1)

    #To get city.
    #Address and city are separated by two spaces, while two word cities has a separation of one between the city words.
    state_begin <- n
    n <- n-3

    rest <- unlist(str_split(str_sub(mailing, 1, n), pattern = "  "))

    address <- rest[1]
    city <- rest[2]
    zip <- zip
    state <- st

    return(c(address, city, zip, state))

  }


  if(col == "geocode"){

      df <- process.geocode(df, col)
      return(df)

  } else{

    #Come from geocode
    geo.code < -c()
    latitude <- c()
    longitude <- c()

    #Come from mailing
    address <- c()
    city <- c()
    zip <- c()
    state <- c()

    for(i in 1:length(df[[col]])){

      location <- df[[col]][i]

      if(grepl(location, pattern = "\n")){

        mailing <- unlist(str_split(location, pattern = "\n"))[1]
        geoPart <- unlist(str_split(location, pattern = "\n"))[2]

        #Mailing
        mailing.stuff <- processMailing(mailing)
        address <- c(address, mailing.stuff[1])
        city <- c(city, mailing.stuff[2])
        zip <- c(zip, mailing.stuff[3])
        state <- c(state, mailing.stuff[4])

        #Geo
        geo <- geoPart
        geo.code <- c(geo.code, geo)

        lat <- as.character(unlist(str_split(geo, pattern = ","))[1])
        long <- as.character(unlist(str_split(geo, pattern = ","))[2])

        lat <- str_sub(lat, 2, nchar(lat))
        long <- str_sub(long, 1, nchar(long)-1)

        latitude <- c(latitude, lat)
        longitude <- c(longitude, long)

      }else{

        #Mailing
        mailing.stuff <- processMailing(mailing)
        address <- c(address, mailing.stuff[1])
        city <- c(city, mailing.stuff[2])
        zip <- c(zip, mailing.stuff[3])
        state <- c(state, mailing.stuff[4])

        #Geocode
        geo.code <- c(geo.code, NA)
        latitude <- c(latitude, NA)
        longitude <- c(longitude, NA)

      }

    }

    df$address <- address
    df$city <- city
    df$zip <- zip
    df$state <- state
    df$geocode <- geo.code
    df$latiude <- latiude
    df$longitude <- longitude

    return(df)

  }

}
