#' Assign CBSA
#'
#' This function assigns the CBSA for a supplied county or Census tract. The
#' assignment will be based on the CBSA definitions that are in place on January
#' 1st of each date by default (assuming only the year is supplied).
#' @param tract Census tract
#' @param county 5-digit county FIPS code
#' @param date Date of OMB defintition to use (default equals current definition)
#' @param use_md Logical indicating if metropolitan division should be returned (default = TRUE)
#' @param only_metro Match only Metropolitan Statistical Areas (default = FALSE)
#' @param assign_nonmetro Logical indicating whether rural areas should be assigned to n
#' a cbsa value of 999SS where SS is the state FIPS code of the area.
#' @export
assign_cbsa <- function(tract, county_fips, date = format(Sys.Date(), '%Y%m'),
                        use_md = TRUE, only_metro = TRUE, assign_nonmetro = FALSE) {
  if (missing(tract) & missing(county_fips))
    stop('Must supply either a tract or county to assign_cbsa')

  # If county FIPS is not present, derive from tract
  if (missing(county_fips)) {
    if (is.character(tract))
      tract <- as.numeric(tract)

    county_fips <- floor(tract / 1e6)
  }

  cbsa_dt <- load_cbsa(date)

  if (only_metro)
    cbsa_dt <- filter(cbsa_dt, is_metro)

  cbsa_dt <- select(cbsa_dt, fips, cbsa, md, is_metro) %>%
    left_join(data.frame(fips = county_fips), ., by = 'fips')

  if (use_md)
    cbsa_dt <- mutate(cbsa_dt, cbsa = ifelse(is.na(md), cbsa, md))

  if (assign_nonmetro)
    cbsa_dt <- mutate(cbsa_dt,
                      cbsa = ifelse(is.na(cbsa),
                                    floor(fips / 1000) + 99900,
                                    cbsa))

  cbsa_dt$cbsa
}

#' Load Median Family Income File
#'
#' Load the Median Family Income file for a given year.
#' @param year 4-digit year
#' @export
load_mfi <- function(year) {
  file_name <- sprintf('%s/data/mfi_definitions_%d.txt',
                       path.package('cbsa'),
                       year)
  if (!file.exists(file_name))
    stop(paste0('Median family income file not found for year ', year))

  read.table(file = file_name, sep = '\t')
}


#' Load CBSA Data.frame
#'
#' This function will load the appropriate CBSA data.frame for a given date.
#' @param date Date for the file to be returned.  Can be a year or a year-month.
#' @export
load_cbsa <- function(date) {
  determine_file_date(date) %>%
    sprintf('%s/data/cbsa_definition_%d.txt', path.package('cbsa'), .) %>%
    read.table(sep = '\t')
}

#' Load NECTA Data.frame
#'
#' This function will load the appropriate NECTA data.frame for a given date.
#' @param date Date for the file to be returned. Can be a year or a year-month.
#' @export
load_necta <- function(date) {
  determine_file_date(date) %>%
    sprintf('%s/data/necta_definition_%d.rds', path.package('cbsa'), .) %>%
    read.table(sep = '\t')
}

#' Determine File Date
#'
#' This function determines the CBSA definitions that were in effect during the
#' date specified. This is an internal function.
#' @param date A numeric date of the format YYYYMM
determine_file_date <- function(date) {
  if (is.character(date))
    date <- as.numeric(date)

  # date < 10000 means it's a year only.  If so, convert to January of that year
  if (date < 10000)
    date <- (date * 100) + 1

  date_list <- list.files(path = sprintf('%s/data', path.package('cbsa')),
                          pattern = 'cbsa_[a-z0-9_]*.txt',
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

#' Assign Income Level
#'
#' This function takes a vector of Census tracts and a year and returns either
#' the relative income of the tract or the income level categorization (i.e., low,
#' moderate, middle, or upper).
#' @param search_tract Numeric vector of Census tract codes (11 digits)
#' @param year A numeric year (YYYY)
#' @param return_label A logical indicating if the income level categorization is
#' to be returned or the relative income (default = TRUE)
#' @return Either the relative income (if return_label == FALSE) or the income level
#' @export
assign_lmi <- function(search_tract, year, return_label = TRUE) {
  if (missing(search_tract))
    stop('Must supply tract to assign_lmi')
  if (missing(year))
    year <- latest_year() %>% as.numeric()

  if (!is.numeric(search_tract) | !is.numeric(year))
    stop('Invalid type (non-numeric) supplied for search_tract or year in assign_lmi')

  # Get the Cenus tract mfi data
  if (year < 2004 | year > latest_year()) {
    stop('LMI areas cannot be assigned for years before 2004.')
  } else if (year < 2012) {
    use_file <- 'dec_2000'
  } else if (year < 2017) {
    use_file <- 'acs_2010'
  } else {
    use_file <- 'acs_2015'
  }

  mfi_data <- file.path(path.package('cbsa'),
                        'data/tract_mfi_levels.txt') %>%
    read.table(sep = '\t') %>%
    filter(file == use_file) %>%
    mutate(cbsa = assign_cbsa(tract = tract,
                              date = (year * 100) + 1,
                              only_metro = FALSE,
                              assign_nonmetro = TRUE)) #%>%
    #mutate(cbsa = ifelse(is.na(cbsa), 99900 + floor(tract / 1e9), cbsa))

  ffiec_data <- load_mfi(year) %>%
    select(cbsa, mfi) %>%
    left_join(mfi_data, ., by = 'cbsa') %>%
    mutate(relative_income = tract_mfi / mfi) %>%
    mutate(income_level = cut(relative_income,
                              breaks = c(0, 0.5, 0.8, 1.2, Inf),
                              labels = c('Low', 'Moderate', 'Middle', 'Upper'),
                              right = FALSE))

  final_data = left_join(data.frame(tract = search_tract), ffiec_data, by="tract")

  if (return_label)
    return(final_data$income_level)

  final_data$relative_income
}

relinc2lmi <- function(x) {
  cut(x / 100,
      breaks = c(0, 0.5, 0.8, 1.2, Inf),
      labels = c('Low', 'Moderate', 'Middle', 'Upper'),
      right = FALSE)
}

latest_year <- function() {
  list.files(path = file.path(path.package('cbsa'),
                              'data'),
             pattern = 'mfi_definitions') %>%
    gsub('.*_([0-9]{4}).*', '\\1', .) %>%
    max(na.rm = TRUE)
}

#' Assign Median Family Income
#'
#' Assigns the appropriate median family income (based on census or HUD estimates).
#' @param cbsa Vector of CBSAs
#' @param year Calendar year of MFI data to use
#' @param use_hud Logical indicating whether HUD estimates are to be used or
#' Census values (default = FALSE)
#' @export
assign_mfi <- function(cbsa, year, use_hud = FALSE) {
  if (missing(cbsa) | missing(year))
    stop('Both CBSA and year must be supplied to assign_mfi.')
  if (!is.numeric(cbsa) | !is.numeric(year))
    stop('Both CBSA and year supplied to assign_mfi must be numeric.')

  load_mfi(year) %>%
    mutate(use_mfi = ifelse(use_hud == TRUE), mfi_hud, mfi) %>%
    select(cbsa, use_mfi) %>%
    right_join(data.frame(cbsa = cbsa)) %>%
    pull(use_mfi)
}

load_distressed <- function(year) {
  file_name <- sprintf('%s/data/distressed_definitions_%d.txt',
                       path.package('cbsa'),
                       as.integer(year))

  read.table(file = file_name, sep = '\t')
}

#' Load Census Data
#'
#' Loads an existing Census data file created by import_census.  The file contains
#' a data.frame with 3 columns:  tract, tract_mfi, and source file.
#' @importFrom tibble as_tibble
#' @export
load_census <- function() {
  file_name <- sprintf('%s/data/tract_mfi_levels.txt',
                       path.package('cbsa'))

  read.table(file = file_name, sep = '\t') %>%
    tibble::as_tibble()
}

