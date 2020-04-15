import_ffiec_census <- function(year) {

  state_nums <- state_fips(use_territories = TRUE)

  # Preallocate the list of state results
  all_states <- vector('list', length = length(state_nums))

  counter <- 0L
  for (s in state_fips(use_territories = TRUE)) {
    counter <- counter + 1L
    my_results <- lapply(c(1:10), process_ffiec_census, s = s, y = year)
    keep_ind <- vapply(my_results, is.null, TRUE)
    my_results <- my_results[!keep_ind]

    if (length(my_results) > 0)
      all_states[counter] <- bind_rows(my_results)
  }

  saveRDS(bind_rows(all_states),
          sprintf('data/census_ffiec_%d.rds',
                  year))
}

process_ffiec_census <- function(c, s, y) {
  spec <- list(state = s,
               county = c,
               year = y)

  # Read the first page
  header_file <- get_html('demographic', spec, 1L)

  if (valid_html(header_file)) {
    num_pages <- get_page_count(header_file)

    ret_val <- lapply(c(1:num_pages), combine_types, spec = spec) %>%
      bind_rows()
  } else {
    ret_val <- NULL
  }

  ret_val
}

combine_types <- function(i, spec) {
  list(get_html('demographic', spec, i) %>%
         extract_table() %>%
         clean_demographic(spec),
       get_html('income', spec, i) %>%
         extract_table() %>%
         clean_income(spec),
       get_html('population', spec, i) %>%
         extract_table() %>%
         clean_population(spec),
       get_html('housing', spec, i) %>%
         extract_table() %>%
         clean_housing(spec)) %>%
    Reduce(function(df1, df2) full_join(df1, df2, by = 'tract'), .)
}

valid_html <- function(in_html) {
  !grepl('Invalid Search', html_text(in_html))
}

get_html <- function(r, spec, p) {
  sprintf('https://www.ffiec.gov/Census/report.aspx?year=%d&state=%d&msa=&county=%03d&tract=&report=%s&page=%d',
          spec$year, spec$state, spec$county, r, p) %>%
    read_html()
}

get_page_count <- function(in_html) {
  in_text <- html_text(in_html)

  if (grepl('Page [0-9]* of', in_text)) {
    ret_val <- stringr::str_extract(in_text, 'Page \\d* of \\d*') %>%
      gsub('Page [0-9]* of ([0-9]*)', '\\1', .) %>%
      as.integer()
  } else {
    ret_val <- 1L
  }
  ret_val
}

extract_table <- function(in_html) {
  html_nodes(in_html, 'table') %>%
    `[[`(7) %>%
    html_table()
}

clean_demographic <- function(df, spec) {
  name_list <- c('tract', 'level', 'relative_income', 'est_area_mfi', 'est_tract_mfi',
                 'tract_mfi', 'pop', 'min_shr', 'min_pop', 'units_oo', 'units_1to4')
  if (spec$year >= 2005L)
    name_list <- append(name_list, 'distressed', after = 2L)

  check_name_list(df, name_list)
  names(df) <- name_list

  # Convert distressed to a boolean or create if necessary (pre-2005)
  if ('distressed' %in% name_list) {
    df$distressed <- (df$distressed == 'Yes')
  } else {
    df <- mutate(df, distressed = as.logical(NA))
  }

  df$tract <- create_tract(df, spec)
  df$year <- spec$year

  select(df, tract, year, distressed)
}

clean_income <- function(df, spec) {
  name_list <- c('tract', 'level', 'area_mfi', 'est_area_mfi', 'poverty_rate',
                 'relative_income', 'tract_mfi', 'est_tract_mfi', 'tract_mhi')

  check_name_list(df, name_list)
  names(df) <- name_list

  df$tract <- create_tract(df, spec)

  # Remove dollar signs and commas and convert to numeric variables
  df$area_mfi <- currency2numeric(df$area_mfi)
  df$est_area_mfi <- currency2numeric(df$est_area_mfi)
  df$tract_mfi <- currency2numeric(df$tract_mfi)
  df$est_tract_mfi <- currency2numeric(df$est_tract_mfi)
  df$tract_mhi <- currency2numeric(df$tract_mhi)

  df
}

clean_population <- function(df, spec) {
  name_list <- c('tract', 'pop', 'share_minority', 'families', 'households', 'pop_white',
                 'pop_minority', 'pop_indian', 'pop_asian', 'pop_black', 'pop_hispanic',
                 'pop_other')

  check_name_list(df, name_list)
  names(df) <- name_list

  df$tract <- create_tract(df, spec)

  df
}

clean_housing <- function(df, spec) {
  name_list <- c('tract', 'units', 'units_1to4', 'house_age', 'in_city', 'units_owner',
                 'units_vacant', 'units_owner_1to4', 'units_renter')

  check_name_list(df, name_list)
  names(df) <- name_list

  df$in_city <- (df$in_city == 'Yes')
  df$tract <- create_tract(df, spec)

  df
}

check_name_list <- function(df, nl) {
  if (length(names(df)) != length(nl))
    sprintf('Invalid name_list length:  %d and %d',
            length(names(df)),
            length(nl)) %>%
    stop()
  invisible(TRUE)
}

create_tract <- function(df, spec) {
  (df$tract * 100) + (spec$state * 1e9) + (spec$county * 1e6)
}

currency2numeric <- function(x) {
  gsub('[$,]', '', x) %>%
    as.numeric()
}
