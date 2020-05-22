#' List of State Names
#'
#' Returns the state names.  Supplements the default variable state.name to include
#' the District of Columbia and the U.S. territories.
#' @param use_territories Should US territories be included with state names (default = TRUE)
#' @export
list_state_names <- function(use_territories = TRUE) {
  ret_val <- c(state.name, 'District of Columbia')

  if (use_territories)
    ret_val <- c(ret_val,
                 'American Samoa',
                 'Federated States of Micronesia',
                 'Guam',
                 'Marshall Islands',
                 'Northern Mariana Islands',
                 'Palau',
                 'Puerto Rico',
                 'U.S. Minor Outlying Islands',
                 'Virgin Islands')

  ret_val

  if (!use_territories)
    ret_val <- ret_val[c(1:50)]

  ret_val
}

#' List of State Abbreviations
#'
#' Supplements the options in state.abb to include DC and the U.S. Territories.
#' @export
list_state_abbs <- function(use_territories = TRUE) {
  ret_val <- c(state.abb, 'DC')

  if (use_territories)
    ret_val <- c(ret_val, c('AS', 'FM', 'GU', 'MH', 'MP', 'PW', 'PR', 'UM', 'VI'))

  ret_val
}

#' List of State FIPs Codes
#'
#' This function returns a list of available state FIPs codes in the same order
#' as the state abbreviations and names.
#' @export
list_state_fips <- function(use_territories = TRUE) {
  ret_val <- c(setdiff(c(1:56), c(11, 3, 7, 14, 43, 52)), 11)

  if (use_territories)
    ret_val <- c(ret_val, c(60, 64, 66, 68, 69, 70, 72, 74, 78))

  ret_val
}

#' Convert State to State FIPS
#'
#' This function takes a state (either the name of the state or the two-character
#' abbreviation) and returns the numeric state FIPS.
#' @param s State as a character vector. Can be state name or abbreviation
#' @param use_name Input variable is the name of the staes (default = FALSE)
#' @export
state2fips <- function(s, use_name = FALSE) {
  abb_list <- list_state_abbs()
  name_list <- list_state_names()
  fips_list <- list_state_fips()

  if (use_name) {
    ret_val <- fips_list[match(toupper(s), toupper(name_list))]
  } else {
    ret_val <- fips_list[match(toupper(s), toupper(abb_list))]
  }
  ret_val
}
