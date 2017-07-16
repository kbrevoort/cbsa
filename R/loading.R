#' Assign CBSA
#'
#' This function assigns the CBSA for a supplied county or Census tract. The
#' assignment will be based on the CBSA definitions that are in place on January
#' 1st of each date by default (assuming only the year is supplied).
#' @param tract Census tract
#' @param county 5-digit county FIPS code
#' @param date Date of OMB defintition to use (default equals current definition)
#' @param only_metro Match only Metropolitan Statistical Areas (default = FALSE)
#' @export
assign_cbsa <- function(tract, county, date = format(Sys.Date(), '%Y%m'), use_md = TRUE) {
  if (missing(tract) & missing(county))
    stop('Must supply either a tract or county to assign_cbsa')

  # Convert the tract to the 5-digit FIPs
  if (!missing(tract))
    county <- floor(tract / 1e6)

  my_data <- load_cbsa(date) %>%
    select(fips, cbsa, md, is_metro) %>%
    left_join(data.frame(fips = county), ., by = 'fips')

  if (only_metro)
    my_data <- filter(is_metro)

  if (use_md)
    mutate(my_data, cbsa = ifelse(is.na(md), cbsa, md))

  my_data[['cbsa']]
}

#' Load Median Family Income File
#'
#' Load the Median Family Income file for a given year.
#' @param year 4-digit year
#' @export
load_mfi <- function(year) {
  file_name <- sprintf('%s/data/mfi_definitions_%d.rds',
                       path.package('cbsa'),
                       year)
  if (!file.exists(file_name))
    stop(paste0('Median family income file not found for year ', year))

  readRDS(file_name)
}


#' Load CBSA Data.frame
#'
#' This function will load the appropriate CBSA data.frame for a given date.
#' @param date Date for the file to be returned.  Can be a year or a year-month.
#' @export
load_cbsa <- function(date) {
  determine_file_date(date) %>%
    sprintf('data/cbsa_definition_%d.rds', .) %>%
    readRDS()
}

#' Load NECTA Data.frame
#'
#' This function will load the appropriate NECTA data.frame for a given date.
#' @param date Date for the file to be returned. Can be a year or a year-month.
#' @export
load_necta <- function(date) {
  determine_file_date(date) %>%
    sprintf('data/necta_definition_%d.rds', .) %>%
    readRDS()
}


determine_file_date <- function(date) {
  if (is.character(date))
    date <- as.numeric(date)

  # date < 10000 means it's a year only.  If so, convert to January of that year
  if (date < 10000)
    date <- (date * 100) + 1

  date_list <- list.files(path = sprintf('%s/data', path.package('cbsa')),
                          pattern = 'cbsa_[a-z0-9_]*.rds',
                          full.names = TRUE) %>%
    extract_yyyymm() %>%
    as.numeric() %>%
    sort()

  if (date < min(date_list))
    stop('OMB Definitions are not available for dates earlier than 200306.')

  if (date > max(date_list)) {
    ret_val <- max(date_list)
  } else {
    ret_val <- date_list[date_list >= date][1]
  }

  ret_val
}
