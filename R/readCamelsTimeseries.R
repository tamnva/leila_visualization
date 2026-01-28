#' Show popup message when clicking the gauge or basin shape file
#'
#' @param timeseries_dir Character; path to time series folder of CAMLES-DE
#' 
#' @return A data frame containing CAMLES-DE time series streamflow data
#'
#' @examples
#'
#' timeseries_dir <- "C:/Users/nguyenta/Documents/LEILA/working_code_documentation/data/camels_de/timeseries"
#' variable_name <- c("discharge_spec_obs", "precipitation_mean")
#' start_date <- as.Date("2001-01-01")
#' end_date <- as.Date("2020-12-31")
#' streamflow_data <- readCamelsTimeseries(timeseries_dir, variable_name, start_date, end_date)
#'
#' @export


# Show a popup at the given location
combineCamelsTimeseries <- function(timeseries_dir) {
  
  files <- list.files(timeseries_dir, full.names = TRUE)
  pb <- txtProgressBar(min = 1, max = length(files), style = 3)
  
  timeseries <- list()
  
  for (i in 1:length(files)){
    
    setTxtProgressBar(pb, i)

    gauge_id <- substr(basename(files[i]), 31, 38)
    
    timeseries[[i]] <- readr::read_csv(files[i], progress = FALSE,
                                  show_col_types = FALSE) %>%
      dplyr::mutate(gauge_id = gauge_id) %>%
      dplyr::relocate(last_col(), .before = 1)
    
  }

  return(dplyr::bind_rows(timeseries))
  
}


