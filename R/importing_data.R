#' Import OMB CBSA Definitions
#'
#' This function will import MSA definitons as published by OMB.
#' @param in_file Location of file to import. The file name must start with
#' 'cbsa' or 'necta', be an Excel file (xls), and have a date string embedded
#' into the title as YYYYMM.
#' @import data.table
#' @importFrom magrittr "%>%"
#' @export
import_definition_file <- function(in_file) {
  # Verify that the data imported is correct
  if (missing(in_file))
    stop('File must be supplied to import_cbsa_definition')
  if (!file.exists(in_file))
    stop(paste0('File not found: ', in_file))

  # Determine the file type
  file_type <- strsplit(basename(in_file), '_')[[1]][1]
  if (!(file_type %in% c('necta', 'cbsa')))
    stop('Unable to determine file type in import_cbsa_definition')
  use_necta <- (file_type == 'necta')

  file_date <- extract_yyyymm(in_file)
  year <- as.integer(substr(file_date, 1, 4))

  if (use_necta) {
    if (as.integer(year) >= 2010) {
      my_data <- process_necta_post2009(in_file)
    } else {
      my_data <- process_necta_pre2010(in_file)
    }
  } else {
    if (year >= 2010) {
      my_data <- process_cbsa_post2009(in_file)
    } else if (year < 2007) {
      my_data <- process_cbsa_pre2007(in_file)
    } else {
      my_data <- process_cbsa_pre2010(in_file)
    }
  }

  setattr(my_data, 'comment', file_date)

  sprintf('data/%s_definition_%s.rds',
          file_type,
          substr(file_date, 1, 6)) %>%
    saveRDS(my_data, .)
  invisible(TRUE)
}

process_necta_pre2010 <- function(in_file) {
  my_date <- extract_yyyymm(in_file)
  if (my_date %in% c('200306', '200312')) {
    skip_rows <- 3L
  } else if (my_date %in% c('200411')) {
    skip_rows <- 8L
  } else {
    skip_rows <- 4L
  }

  my_data <- in_file %>%
    readxl::read_xls(skip = skip_rows,
                     col_names = c('necta', 'nectad', 'cnecta', 'necta_name',
                                   'necta_type', 'status', 'nectad_name', 'cnecta_name',
                                   'state_fips', 'county_fips', 'mcd', 'component_name'),
                     col_types = 'text') %>%
    filter(!is.na(necta_name)) %>%
    convert2numeric() %>%
    mutate(is_metro = (necta_type == 'Metropolitan NECTA')) %>%
    mutate(fips_mcd = as.numeric(paste0(state_fips, county_fips, mcd))) %>%
    select(necta, nectad, cnecta, necta_name, nectad_name, cnecta_name,
           is_metro, fips_mcd, component_name)
}

process_necta_post2009 <- function(in_file) {
  my_data <- in_file %>%
    readxl::read_xls(skip = 3L,
                     col_names = c('necta', 'nectad', 'cnecta', 'necta_name',
                                   'necta_type', 'nectad_name', 'cnecta_name', 'component_name',
                                   'state_fips', 'county_fips', 'mcd'),
                     col_types = 'text') %>%
    filter(!is.na(necta_name)) %>%
    convert2numeric() %>%
    mutate(is_metro = (necta_type == 'Metropolitan NECTA')) %>%
    mutate(fips_mcd = as.numeric(paste0(state_fips, county_fips, mcd))) %>%
    select(necta, nectad, cnecta, necta_name, nectad_name, cnecta_name,
           is_metro, fips_mcd, component_name)
}

process_cbsa_pre2007 <- function(in_file) {
  my_date <- extract_yyyymm(in_file)
  if (my_date %in% c('200411')) {
    skip_rows <- 8L
  } else if (my_date %in% c('200512', '200612')) {
    skip_rows <- 4L
  } else {
    skip_rows <- 3L
  }

  my_data <- in_file %>%
    readxl::read_xls(skip = skip_rows,
                     col_names = c('cbsa', 'md', 'csa', 'cbsa_name',
                                   'cbsa_type', 'status', 'md_name', 'csa_name',
                                   'component_name', 'state', 'fips'),
                     col_types = 'text') %>%
    filter(!is.na(cbsa_name)) %>%
    convert2numeric() %>%
    mutate(is_central = NA) %>%
    mutate(is_metro = (cbsa_type == 'Metropolitan Statistical Area')) %>%
    select(cbsa, md, csa, cbsa_name, md_name, csa_name, is_metro, fips,
           component_name, is_central)
}

#' Process CBSA File
#'
#' This is meant to be an internal function called by import_cbsa_definition only.
process_cbsa_pre2010 <- function(in_file) {
  # The existence of in_file has already been tested
  my_data <- in_file %>%
    readxl::read_xls(skip = 4L,
                     col_names = c('cbsa', 'md', 'csa', 'cbsa_name',
                                   'cbsa_type', 'status', 'md_name', 'csa_name',
                                   'component_name', 'state', 'fips', 'central_fl'),
                     col_types = 'text') %>%
    filter(!is.na(cbsa_name)) %>%
    convert2numeric() %>%
    mutate(is_metro = (cbsa_type == 'Metropolitan Statistical Area')) %>%
    mutate(is_central = (central_fl == 'Central')) %>%
    select(cbsa, md, csa, cbsa_name, md_name, csa_name, is_metro, fips,
           component_name, is_central)
}

process_cbsa_post2009 <- function(in_file) {
  # The existence of in_file has already been tested
  my_data <- in_file %>%
    readxl::read_xls(skip = 4L,
                     col_names = c('cbsa', 'md', 'csa', 'cbsa_name',
                                   'cbsa_type', 'md_name', 'csa_name', 'component_name',
                                   'state', 'state_fips', 'county_fips', 'central_fl'),
                     col_types = 'text') %>%
    filter(!is.na(cbsa_name)) %>%
    convert2numeric() %>%
    mutate(is_metro = (cbsa_type == 'Metropolitan Statistical Area')) %>%
    mutate(is_central = (central_fl == 'Central')) %>%
    mutate(fips = as.numeric(paste0(state_fips, county_fips))) %>%
    select(cbsa, md, csa, cbsa_name, md_name, csa_name, is_metro, fips,
           component_name, is_central)
}

#' List of State Names
#'
#' Returns the state names.  Supplements the default variable state.name to include
#' the District of Columbia and the U.S. territories.
state_names <- function() {
  c(state.name,
    c('District of Columbia',
      'American Samoa',
      'Federated States of Micronesia',
      'Guam',
      'Marshall Islands',
      'Commonwealth of the Northern Mariana Islands',
      'Palau',
      'Puerto Rico',
      'U.S. Minor Outlying Islands',
      'U.S. Virgin Islands'))
}

#' List of State Abbreviations
#'
#' Supplements the options in state.abb to include DC and the U.S. Territories.
state_abbs <- function() {
  c(state.abb, c('DC', 'AS', 'FM', 'GU', 'MH', 'MP', 'PW', 'PR', 'UM', 'VI'))
}

#' List of State FIPs Codes
#'
#' This function returns a list of available state FIPs codes in the same order
#' as the state abbreviations and names.
state_fips <- function() {
  c(setdiff(c(1:56), c(11, 3, 7, 14, 43, 52)),
    c(11, 60, 64, 66, 68, 69, 70, 72, 74, 78))
}

#' Convert State to State FIPS
#'
#' This function takes a state (either the name of the state or the two-character
#' abbreviation) and returns the numeric state FIPS.
#' @param s State as a character vector. Can be state name or abbreviation
#' @param use_name Input variable is the name of the staes (default = FALSE)
#' @export
state2fips <- function(s, use_name = FALSE) {
  abb_list <- state_abbs()
  name_list <- state_names()
  fips_list <- state_fips()

  if (use_name) {
    ret_val <- fips_list[match(s, name_list)]
  } else {
    ret_val <- fips_list[match(s, abb_list)]
  }
  ret_val
}

#' Extract the Date String from the File Name
#'
#' This function extracts the 6 digit date (YYYYMM) from the name of the original
#' input file. The files published by OMB do not use this naming convention --
#' the files have non-unique names like list9.xls.  I created these file names
#' so that each input file is unique.
extract_yyyymm <- function(x) {
  gsub('.*_([0-9]{6}).*', '\\1', x)
}

#' Convert to Numeric
#'
#' This function will convert all of the numeric variables from character into
#' numeric.  The reason I cannot just specify numeric formats for these variables
#' in the read_xls function is that the input files have notes at the bottom that
#' are read into some of the numeric variables, resulting in an error.
convert2numeric <- function(df) {
  numeric_vars <- intersect(c('cbsa', 'md', 'csa', 'necta', 'nectad', 'cnecta', 'fips'),
                            names(df))
  for(var in numeric_vars)
    df[[var]] <- as.numeric(df[[var]])

  df
}
