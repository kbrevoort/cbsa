#' Import FFIEC Median Family Incomes
#'
#' This function loans the median family incomes for MSAs and for the nonmetropolitan
#' state areas necessary to determine the relative income level of a Census tract.
#' These figures are published each year by the Federal Financial Institutions
#' Examination Council (FFIEC).
#' @param year 4-digit year of file to be read in
import_ffiec <- function(year) {
  if (is.character(year))
    year <- as.numeric(year)

  file_name <- sprintf('%s/data/original_data/msa%02dinc.xls',
                       find.package('cbsa'),
                       year %% 100)
  out_file <- sprintf('%s/data/mfi_definitions_%04d.txt',
                      find.package('cbsa'),
                      as.integer(year))

  if (!file.exists(file_name))
    stop(paste0('Could not find FFIEC file for year ', year))

  all_out <- readxl::read_xls(file_name,
                   skip = 2L,
                   col_names = c('cbsa', 'cbsa_name', 'mfi', 'mfi_hud'),
                   col_types = 'text',
                   na = 'NULL') %>%
    mutate(mfi = as.numeric(gsub(',', '', mfi)),
           mfi_hud = as.numeric(gsub(',', '', mfi_hud)),
           cbsa = as.numeric(cbsa))

  non_metro <- filter(all_out, cbsa == 99999) %>%
    mutate(cbsa = state2fips(gsub('nonmetro portion of (.*)', '\\1', cbsa_name),
                                   use_name = TRUE) + 99900)

  list(filter(all_out, cbsa < 99999), non_metro) %>%
    bind_rows() %>%
    write.table(file = out_file, sep = '\t')

  invisible(TRUE)
}

#' Import OMB Definitions
#'
#' This function will import CBSA and NECTA definitons as published by OMB.
#' @param in_file Location of file to import. The file name must start with
#' 'cbsa' or 'necta', be an Excel file (xls), and have a date string embedded
#' into the title as YYYYMM.
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @export
import_omb <- function(in_file) {
  # Verify that the data imported is correct
  if (missing(in_file))
    stop('File must be supplied to import_cbsa_definition')
  if (!file.exists(in_file))
    stop(paste0('File not found: ', in_file))

  # Determine the file type
  file_type <- strsplit(basename(in_file), '_')[[1]][1]
  if (!(file_type %in% c('necta', 'cbsa')))
    stop('Unable to determine if input file is for NECTA or CBSA in import_cbsa_definition')

  use_necta <- (file_type == 'necta')
  if (use_necta) {
    out_data <- process_necta(in_file)
  } else {
    out_data <- process_cbsa(in_file)
  }
  comment(out_data) <- extract_yyyymm(in_file)

  file.path(find.package('cbsa'),
            sprintf('data/%s_definition_%s.txt',
                    file_type,
                    comment(out_data))) %>%
    write.table(x = out_data, file = ., sep = '\t')

  invisible(TRUE)
}

#' Returns the specifications of the NECTA definition file
#'
#' This is an internal function used by import_omb.
get_necta_specs <- function(date_stamp) {
  in_year <- as.integer(substr(date_stamp, 1, 4))
  if (in_year < 2010) {
    if (in_year < 2004) {
      skip_rows <- 3L
    } else if (in_year < 2005) {
      skip_rows <- 8L
    } else {
      skip_rows <- 4L
    }
    col_names <- c('necta', 'nectad', 'cnecta', 'necta_name',
                   'necta_type', 'status', 'nectad_name', 'cnecta_name',
                   'state_fips', 'county_fips', 'mcd', 'component_name')
  } else {
    skip_rows <- 3L
    col_names <- c('necta', 'nectad', 'cnecta', 'necta_name',
                   'necta_type', 'nectad_name', 'cnecta_name', 'component_name',
                   'state_fips', 'county_fips', 'mcd')
  }

  list(skip_rows = skip_rows, col_names = col_names)
}

#' Process NECTA file
#'
#' This internal function reads in the NECTA definition file from OMB.
#' @import dplyr
#' @param in_file Location of file to import. The file name must start with
#' 'cbsa' or 'necta', be an Excel file (xls), and have a date string embedded
#' into the title as YYYYMM.
process_necta <- function(in_file) {
  my_date <- extract_yyyymm(in_file)

  specs <- get_necta_specs(my_date)
  my_data <- readxl::read_xls(in_file,
                              skip = specs$skip_rows,
                              col_names = specs$col_names,
                              col_types = 'text') %>%
    filter(!is.na(necta_name)) %>%
    convert2numeric() %>%
    mutate(is_metro = (necta_type == 'Metropolitan NECTA'),
           fips_mcd = as.numeric(paste0(state_fips, county_fips, mcd))) %>%
    select(necta, nectad, cnecta, necta_name, nectad_name, cnecta_name,
           is_metro, fips_mcd, component_name)
}

get_cbsa_specs <- function(date_stamp) {
  in_year <- as.integer(substr(date_stamp, 1, 4))
  if (in_year < 2007L) {
    if (date_stamp == c('200411')) {
      skip_rows <- 8L
    } else if (date_stamp %in% c('200512', '200612')) {
      skip_rows <- 4L
    } else {
      skip_rows <- 3L
    }
    col_names <- c('cbsa', 'md', 'csa', 'cbsa_name',
                   'cbsa_type', 'status', 'md_name', 'csa_name',
                   'component_name', 'state', 'fips')
  } else if (in_year < 2010) {
    skip_rows <- 4L
    col_names <- c('cbsa', 'md', 'csa', 'cbsa_name',
                   'cbsa_type', 'status', 'md_name', 'csa_name',
                   'component_name', 'state', 'fips', 'central_fl')
  } else {
    skip_rows <- 4L
    col_names <- c('cbsa', 'md', 'csa', 'cbsa_name',
                   'cbsa_type', 'md_name', 'csa_name', 'component_name',
                   'state', 'state_fips', 'county_fips', 'central_fl')
  }
  list(skip_rows = skip_rows, col_names = col_names)
}

#' Process CBSA file
#'
#' This internal function reads in the CBSA definition file from OMB.
#' @import dplyr
#' @importFrom magrittr "%>%"
process_cbsa <- function(in_file) {
  my_date <- extract_yyyymm(in_file)
  specs <- get_cbsa_specs(my_date)

  my_data <- in_file %>%
    readxl::read_xls(skip = specs$skip_rows,
                     col_names = specs$col_names,
                     col_types = 'text') %>%
    filter(!is.na(cbsa_name)) %>%
    convert2numeric() %>%
    mutate(is_metro = (cbsa_type == 'Metropolitan Statistical Area'))

  # The central_fl variable is only available in later years
  if ('central_fl' %in% names(my_data)) {
    my_data$is_central = (my_data$central_fl == 'Central')
  } else {
    my_data$is_central = as.logical(NA)
  }

  # In later years, the 5-digit fips has been replaced by separate
  # state and county fips variables.
  if (!('fips' %in% names(my_data)))
    if (all(c('state_fips', 'county_fips') %in% names(my_data))) {
      my_data$fips <- as.numeric(paste0(my_data$state_fips, my_data$county_fips))
    } else {
      my_data$fips <- as.numeric(NA)
    }

  select(my_data,
         cbsa, md, csa, cbsa_name, md_name, csa_name, is_metro, fips,
         component_name, is_central)
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

#' Import Census Data
#'
#' This function uses the tidycensus package to import data on median family incomes
#' at the Census tract level from either the 2000 Decennial Census or the appropriate
#' 5-year American Community Survey.
import_census <- function() {
  if (!('tidycensus' %in% installed.packages()))
    stop('import_census function only works when tidycensus is installed.')

  acs_data <- lapply(c(2010L, 2015L), download_acs) %>%
    bind_rows()

  dec_data <- purrr::map_df(state_fips(use_territories = FALSE),
                            ~ suppressMessages(tidycensus::get_decennial(geography = 'tract',
                                                                         variables = 'P077001',
                                                                         year = 2000L,
                                                                         sumfile = 'sf3',
                                                                         state = .x))) %>%
    select(GEOID, value) %>%
    rename(tract = GEOID, tract_mfi = value) %>%
    arrange(tract) %>%
    mutate(file = 'dec_2000')

  list(acs_data, dec_data) %>%
    bind_rows() %>%
    mutate(tract = as.numeric(tract)) %>%
    write.table(file = file.path(find.package('cbsa'),
                                 'data/tract_mfi_levels.txt'),
                sep = '\t')

  invisible(TRUE)
}

download_acs <- function(endyear) {
  lapply(state_fips(use_territories = FALSE),
         function(x) tidycensus::get_acs('tract', 'B19113_001E', year = endyear, state = x)) %>%
    bind_rows() %>%
    select(GEOID, estimate) %>%
    rename(tract = GEOID, tract_mfi = estimate) %>%
    mutate(file = sprintf('acs_%d', endyear))
}

#' Import Distressed Areas
#'
#' Reads in information on distressed or underserved tracts.
#' @param year 4-digit year of file to be read
#' @importFrom readxl read_xls
#' @importFrom dplyr rename mutate select
import_distressed <- function(year) {
  if (is.character(year))
    year <- as.numeric(year)

  file_name <- sprintf('%s/data/original_data/%ddistressedorunderservedtracts.xls',
                       find.package('cbsa'),
                       as.integer(year))
  out_file <- sprintf('%s/data/distressed_definitions_%d.txt',
                      find.package('cbsa'),
                      as.integer(year))

  if (!file.exists(file_name))
    stop(paste0('Could not find distressed definitions for year ', year))

  all_out <- readxl::read_xls(file_name,
                              skip = 2L,
                              col_types = 'text') %>%
    rename('state' = 'STATE CODE',
           'county' = 'COUNTY CODE',
           'code' = 'TRACT CODE',
           'pop_loss' = 'POPULATION LOSS',
           'remote_rural' = 'REMOTE RURAL',
           'county_name' = 'COUNTY NAME',
           'state_name' = 'STATE NAME') %>%
    mutate(poverty = ifelse(!is.na(POVERTY) & POVERTY == 'X', TRUE, FALSE)) %>%
    mutate(unemployment = ifelse(!is.na(UNEMPLOYMENT) & UNEMPLOYMENT == 'X', TRUE, FALSE)) %>%
    mutate(pop_loss = ifelse(!is.na(pop_loss) & pop_loss == 'X', TRUE, FALSE)) %>%
    mutate(remote_rural = ifelse(!is.na(remote_rural) & remote_rural == 'X', TRUE, FALSE)) %>%
    mutate(code = round(as.numeric(code) * 100)) %>%
    mutate(tract = sprintf('%02d%03d%06d',
                           as.integer(state),
                           as.integer(county),
                           as.integer(code))) %>%
    mutate(tract = as.numeric(tract)) %>%
    select(tract, poverty, unemployment, pop_loss, remote_rural)

  write.table(all_out,
              file = out_file,
              sep = '\t')
  invisible(TRUE)
}
