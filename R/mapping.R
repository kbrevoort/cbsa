#' Source TIGER Map Years
#'
#' Looks up the range of years for which TIGER map data are available
#' online.
#' @return Integer vector of years for which TIGER line data are available
#' @importFrom RCurl getURL
#' @importFrom stringr str_split str_extract
source_tiger_map_years <- function() {
  url <- 'ftp://ftp2.census.gov/geo/tiger/'  # The last backslash is crucial

  file_list <- RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE) %>%
    stringr::str_split('\n') %>%
    unlist()

  stringr::str_extract(file_list, 'GENZ\\d{4}') %>%
    na.omit() %>%
    as.character() %>%
    substring(5) %>%
    as.integer()
}

source_tiger_shapefile <- function(geography, year, resolution = '20m') {
  if (!'sp' %in% installed.packages())
    stop('The sp package is required to call soure_tiger_shapefile. Please install this package first.')

  if (!is.character(geography) |
      !geography %in% c('state', 'county', 'cbsa', 'metro', 'micro', 'csa'))
    stop('Invalid geo value supplied to source_tiger_shapefiles')
  #if (!is.numeric(year) | !year %in% source_tiger_map_years())
  #  stop('Invalid year supplied to source_tiger_shapefiles.')
  if (!resolution %in% c('500k', '5m', '20m'))
    stop('Invalid resolution supplied to source_tiger_shapefiles.')

  geo <- ifelse(geography %in% c('micro', 'metro'), 'cbsa', geography)
  base_name <- sprintf('cb_%i_us_%s_%s', year, geo, resolution)

  if (!file.exists(file.path(get_tiger_data_dir(), paste0(base_name, '.rds')))) {
    shapes <- download_and_save_shapefile(base_name)

    if (geo != 'state') {
      state_base <- sprintf('cb_%i_us_state_%s', year, resolution)
      if (!file.exists(file.path(get_tiger_data_dir(), paste0(state_base, '.rds'))))
        invisible(download_and_save_shapefile(state_base))
    }
  } else shapes <- readRDS(file.path(get_tiger_data_dir(), paste0(base_name, '.rds')))

  alaska_shp <- adjust_states(shapes, 'alaska', geo, year, resolution)
  hawaii_shp <- adjust_states(shapes, 'hawaii', geo, year, resolution)

  ret_shp <- shapes[get_state_ind(shapes, 'excluded', geo), ] %>%
    rbind(alaska_shp, hawaii_shp) %>% #, hawaii_shp) %>%
    # convert from EPSG2163 to (US National Atlas Equal Area) WGS84
    sp::spTransform(sp::CRS("+init=epsg:4326")) %>%
    sf::st_as_sf() %>%
    # set prejection to US National Atlas Equal Area
    sf::st_transform(crs = 2163) #%>%
    #select(GEOID, NAME)

  ret_shp <- filter(ret_shp, !get_state_ind(ret_shp, 'territories', geo))

  if (geography %in% c('metro', 'micro')) {
    code <- ifelse(geography == 'metro', 'M1', 'M2')
    ret_shp <- filter(ret_shp, LSAD == code)
  }
  select(ret_shp, GEOID, NAME)
}

#' Adjust States
#'
#' Extracts the polygons in the supplied shape file that correspond to the state
#' specified, and adjusts them to appear to the lower-left of the continental U.S.
#' @param shapes A `SpatialPolygonsDataFrame`
#' @param state Character scalar equal to either 'alaska' or 'hawaii'
#' @param geo Character vector giving the geography of the shapefile (e.g., 'state',
#' 'cbsa')
#' @param resolution Character scalar given the resolution of the shape file
adjust_states <- function(shapes, state, geo, year, resolution) {
  if (!'sp' %in% installed.packages())
    stop('The sp package is required to run the adjust_states function. Please install this package first.')

  if (state == 'alaska') {
    scale <- 2L
    shift <- c(-2400000, -2500000) #c(-2600000, -2300000)
    center <- c(-4360619, 1466123)
  } else if (state == 'hawaii') {
    scale = 0.8
    shift <- c(-0800000, -2363000)
    center <- c(-5761986, -2361733)
  } else stop('Cannot make adjustments for states other than Alaska and Hawaii.')

  this_shp <- shapes[get_state_ind(shapes, state, geo), ]
  out_shp <- maptools::elide(this_shp, rotate = -35L, center = center)

  if (geo != 'state') {
    temp <- readRDS(sprintf('%s/cb_%i_us_state_%s.rds',
                            get_tiger_data_dir(),
                            year, resolution))
    # Rotate to match out_shp
    temp <- maptools::elide(temp[get_state_ind(temp, state, 'state'), ],
                            rotate = -35L,
                            center = center)
    out_shp@bbox <- temp@bbox
  }

  out_shp <- maptools::elide(out_shp, scale = max(apply(sp::bbox(out_shp), 1, diff)) / scale) %>%
    maptools::elide(shift = shift)

  sp::proj4string(out_shp) <- sp::proj4string(this_shp)

  out_shp
}

#' Get State Indicator
#'
#' Determines which rows of a SpatialPolygonsDataFrame corresponds to the states
#' specified in the function call.
#' @param shapes A SpatialPolygonsDataFrame
#' @param state Character scalar giving the name of the geography to identify (e.g.,
#' alaska, hawaii, excluded, teritories)
#' @param geo Character vector giving the geography of the shape file (e.g, state,
#' cbsa)
#' @return Boolean vector indicating which rows correspond to the states specified
#' in the function call.
#' @importFrom stringr str_split str_trim
#' @importFrom purrr map_lgl
#' @importFrom tibble as_tibble
#' @importFrom dplyr pull
get_state_ind <- function(shapes, state, geo) {
  if (geo %in% c('state', 'county')) {
    if (state == 'alaska') {
      ind <- shapes$STATEFP == '02'
    } else if (state == 'hawaii') {
      ind <- shapes$STATEFP == '15'
    } else if (state == 'excluded') {
      ind <- !(substr(shapes$GEOID, 1, 2) %in% c("02", "15", "60", "66", "69", "72", "78"))
    } else if (state == 'territories') {
      ind <- substr(shapes$GEOID, 1, 2) %in% c('60', '66', '69', '72', '78')
    } else stop('Invalid state type submitted to get_state_ind.')
  } else if (geo %in% c('cbsa', 'csa')) {
    if (state == 'alaska') {
      my_pattern <- 'AK'
    } else if (state == 'hawaii') {
      my_pattern = 'HI'
    } else if (state == 'excluded') {
      # This will create the list of non-excluded states
      my_pattern <- setdiff(list_state_abbs(FALSE), c('AK', 'HI'))
    } else if (state == 'territories') {
      my_pattern <- c('AS', 'GU', 'MP', 'PR', 'VI')
    } else stop('Invalid state supplied to get_states_ind.')

    ind <- stringr::str_split(shapes$NAME, ',', simplify = TRUE) %>%
      #tibble::as_tibble(.name_repair = 'minimal') %>%
      as.data.frame() %>%
      pull(2) %>%
      stringr::str_trim() %>%
      stringr::str_split('-') %>%
      purrr::map_lgl(~length(intersect(.x, my_pattern)) > 0)
  } else stop('Invalid geo supplied to get_state_ind.')

  ind
}

#' Download and Save Shapefile
#'
#' If the RDS file is not found for the required geography, this function downloads
#' the required file from the Census website and creates the necessary file.
#'
#' This function requires `{rgdal}` which does not work on some of the machines
#' I use. If the package is not found, the function will return an error.
#' @param basename Character scalar giving the base name of the geographic file
#' to read.
#' @return A `SpatialPolygonsDataFrame` (from the `sp` package) that includes the
#' contents of the TIGER line file converted to a format usable in R.
#' @importFrom rgdal readOGR
#' @importFrom sp spTransform CRS
download_and_save_shapefile <- function(basename) {

  if (!'rgdal' %in% installed.packages())
    stop('The function download_and_save_shapefile requires the rgdal package.  To run this function, first install that package.')
  if (!'sp' %in% installed.packages())
    stop('The function download_and_save_shapefile requires the sp package. To run this function, first install the sp package.')

  year <- substr(basename, 4, 7)
  url <- sprintf('http://www2.census.gov/geo/tiger/GENZ%s/shp/%s.zip',
                 year, basename)

  temp <- tempfile(fileext = '.zip')
  download.file(url, temp)

  temp_dir <- tempdir()
  unzip(temp, exdir = temp_dir)

  shapes <- rgdal::readOGR(dsn = temp_dir, layer = basename, verbose = FALSE) %>%
      sp::spTransform(sp::CRS('+init=epsg:2163'))

  saveRDS(shapes, file.path(get_tiger_data_dir(), paste0(basename, '.rds')))

  shapes
}

get_tiger_data_dir <- function() {
  cbsa_path <- find.package('cbsa')

  if (file.exists(file.path(cbsa_path, 'data/tiger'))) {
    ret_dir <- file.path(cbsa_path, 'data/tiger')
  } else if (file.exists(file.path(cbsa_path, 'inst/data/tiger'))) {
    ret_dir <- file.path(cbsa_path, 'inst/data/tiger')
  } else stop('TIGER data subdirectory not found.')

  ret_dir
}

