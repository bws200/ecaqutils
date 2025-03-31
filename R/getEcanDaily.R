#' Title
#'
#' @description Retrieved daily data from Ecan website for all sites
#' @param from_date
#' @param to_date
#'
#' @returns Daiy data from all sites available
#' @export
#'
#' @examples
#' getEcandaily(from_date = "01/01/2025", to_date = "31/01/2025")
getEcanDaily <- function(from_date, to_date) {

  # Convert type and add a day to account for days between
  from_date <- lubridate::dmy(from_date)
  to_date <- lubridate::dmy(to_date) + 1

  # Convert new to_date to character for URL
  to_date_char <- to_date |>
    format("%d/%m/%Y") |>
    as.character()

  # Check time length
  print("Time between start and end dates is...")

  # Create time interval
  time_interval <- lubridate::interval(
    start = from_date,
    end = to_date
  )

  # Time length in years, days

  message("Time interval is...")
  message(
    lubridate::time_length(time_interval, "years"),
    " years OR"
  )
  message(
    lubridate::time_length(time_interval, "months"),
    " months OR"
  )

  message(
    lubridate::time_length(time_interval, "days"),
    " days"
  )

  # Now we need to get a list of all stations available via the website which uses another procedure. Not all stations will have data available but this is taken care of later by ignoring the return if the httr request returns an http error.

  # URL for getting list of available stations
  response_stations <- httr::GET(
    'http://data.ecan.govt.nz/data/23/Air/Air%20quality%20sites%20monitored/CSV'
  )

  # Basic list of stations as .csv file using URL above
  station_id_list <- httr::content(
    response_stations,
    encoding = "UTF-8",
    show_col_types = FALSE
  ) |>
    # Convert to data frame
    data.frame() |>
    dplyr::mutate(
      LatestDateTime = lubridate::dmy_hms(LatestDateTime)
    )

  # Now can continue on with getting the data via the website.

  # This is the base URL we want to use before adding parameters
  url_1 <- "https://data.ecan.govt.nz:443/data/98/Air/"
  url_2 <- "Air%20quality%20data%20for%20a%20monitored%20site%20(daily)/CSV?"
  base_url = paste0(url_1, url_2)

  # Get the date into the right format for URL
  from_date_url <- stringr::str_replace_all(from_date,"/","%2F")
  to_date_url <- stringr::str_replace_all(to_date,"/","%2F")

  # Now can make the actual request in a loop to retrieve data for all stations which have available data via the ecan website.


  # Initialise datalist to be populated inside the for loop
  datalist = list()

  # For loop to make request of each station
  for (ii in station_id_list$SiteNo) {

    # Make basic URL string
    URL_string <- paste0(
      base_url, "SiteID=", as.character(ii), "&",
      "StartDate=", as.character(from_date_url), "&",
      "EndDate=", as.character(to_date_url)
    )

    # Encode URL string as URL
    URL_request <- utils::URLencode(URL_string)

    # Send URL request message
    response <- httr::GET(URL_request)

    # Stop if response code is not OK. Could include other codes in future
    if (response$status_code != 200) {
      next
    }

    # Response for that station received
    dat_raw <- httr::content(
      response,
      encoding = "UTF-8",
      show_col_types = FALSE,
      as = "text"
    )

    # Parse text to data table
    dat <- read.table(
      text = dat_raw,
      sep = ",",
      header = TRUE,
      stringsAsFactors = FALSE
    )

    # Simplify column names
    names(dat) <- gsub(x = names(dat), pattern = "\\.", replacement = "")

    # Different variables at each station
    dat_longer <- tidyr::pivot_longer(
      dat,
      !c(DateTime, StationName)
    ) |>
      dplyr::mutate(
        value = janitor::round_half_up(value, 1)
      )

    # Change date format
    dat_longer$DateTime <- lubridate::ymd(dat_longer$DateTime)

    # add it tolist
    datalist[[ii]] <- dat_longer
  }

  # Combine data into one big table as a data frame
  df_final_long = do.call(rbind, datalist)

  # This is handy for checking with database to more significant figures.
  options(pillar.sigfig = 7)

  return(df_final_long)

}


